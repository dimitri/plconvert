;;;
;;; Given the parser output (an AST using internal reprensentation nodes as
;;; defined in src/structs.lisp), prepare the AST to be output as a PL/pgSQL
;;; function.
;;;
;;; The following things need to happen:
;;;
;;;   - convert data types from Oracle to PostgreSQL
;;;   - convert magic constants such as SYSDATE
;;;   - take care of variable scope (package references, global ones)
;;;   - rewrite function signatures using both OUT and RETURN
;;;   - rewrite function calls to the same functions (OUT+RETURN)
;;;   - rewrite || expressions into a function call on format
;;;   - rewrite COMMIT/ROLLBACK into comments
;;;   - rewrite lone funcall to PERFORM
;;;

#|
TODO - see about custom exceptions
  http://www.postgresql.org/docs/9.3/static/plpgsql-control-structures.html#PLPGSQL-ERROR-TRAPPING
  http://www.postgresql.org/docs/9.3/static/plpgsql-errors-and-messages.html
|#

(in-package #:plconvert)

(defvar *analyze-package-specs*
  '((set-ora-package                . (package-spec))
    (qualify-fun-and-proc-names     . (decl-fun decl-proc))
    (convert-data-type              . (data-type))
    (collect-package-spec-variables . (decl-var))
    (collect-package-spec-funs      . (decl-fun))
    (collect-funs-with-out+return   . (decl-fun)))
  "Analyze package specifications and prepare dynamic bindings.")

(defvar *convert-from-oracle*
  '((convert-data-type            . (data-type))
    (process-package-vars         . (pl-return
                                     assignment
                                     pl-funcall))
    (set-ora-package              . (package-body))
    (set-current-function         . (fun proc))
    (qualify-fun-and-proc-names   . (fun proc))
    (out+return-to-returns-record . (fun))
    (out+return-call-sites        . (pl-funcall))
    (funcall-to-perform           . (code
                                     pl-if
                                     pl-for pl-forall
                                     pl-case pl-case-when
                                     pl-exception-when)))
  "Convert rules from Oracle to PostgreSQL")

(defvar *oracle-constants*
  (loop :for (ora pg default type)
     :in '(("SYSDATE" "CURRENT_TIMESTAMP" "CURRENT_TIMESTAMP" "timestamptz"))
     :for data-type := (make-data-type :cname (make-cname :attribute type))
     :collect (cons (make-qname :name ora)
                    (make-decl-var :name pg
                                   :default (make-qname :name pg)
                                   :type data-type))))

(defun add-oracle-constants (hash-table)
  "Add oracle constants to given HASH-TABLE."
  (loop :for (name . value) :in *oracle-constants*
     :do (setf (gethash name hash-table) value)))

(defun plsql-to-plpgsql (body list-of-package-specs)
  "Convert raw parsetree to a PL/pgSQL compatible parse tree."
  (let* ((*current-ora-package*  nil)   ; limit scope of changes
         (*current-function*     nil)   ; to there dynamic bindings
         (*packages-vars*        (make-hash-table :test 'equalp))
         (*packages-funs*        (make-hash-table :test 'equalp))
         (*funs-with-out+return* (make-hash-table :test 'equalp)))

    ;; analyze package specifications and prepare context for converting the
    ;; stored procedures and function...
    (loop :for spec :in list-of-package-specs
       :do (walk-apply spec *analyze-package-specs*))

    ;; add the oracle constants to the variables to transform
    (add-oracle-constants *packages-vars*)

    ;; now convert Oracle PL code to something that PostgreSQL might accept
    (walk-apply body *convert-from-oracle*)

    (values *packages-vars*
            *packages-funs*
            *funs-with-out+return*
            ;; body
            )))

;;;
;;; The *current-ora-package* needs to be set so that we can qualify
;;; function and proc names.
;;;
(defun set-ora-package (parsetree)
  "Get current package name for reuse later in the processing."
  (setf *current-ora-package*
        (typecase parsetree
          (package-body (package-body-qname parsetree))
          (package-spec (package-spec-qname parsetree)))))

(defun set-current-function (fun-or-proc)
  "Set the current function so that we can easily change the local
   declaration list from somewhere in the processing."
  (setf *current-function* fun-or-proc))

(defun qualify-fun-and-proc-names (parsetree)
  "Change function and procedure names into qualified names."
  (let ((current-schema  (qname-package *current-ora-package*))
        (current-package (qname-name *current-ora-package*)))
    (typecase parsetree
      (fun   (setf (fun-name parsetree)
                   (make-qname :schema current-schema
                               :package current-package
                               :name (fun-name parsetree))))

      (decl-fun   (setf (decl-fun-name parsetree)
                        (make-qname :schema current-schema
                                    :package current-package
                                    :name (decl-fun-name parsetree))))

      (proc   (setf (proc-name parsetree)
                    (make-qname :schema current-schema
                                :package current-package
                                :name (proc-name parsetree))))

      (decl-proc   (setf (decl-proc-name parsetree)
                         (make-qname :schema current-schema
                                     :package current-package
                                     :name (decl-proc-name parsetree)))))))


