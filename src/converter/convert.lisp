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

(defvar *current-ora-package* nil
  "Defined when parsing a Package Body or a Package Specs node.")

(defvar *current-function* nil
  "Defined when parsing a function definition.")

(defvar *packages-vars* nil
  "Current package level variables and values, in a hash table.")

(defvar *funs-with-out+return* nil
  "List of functions that needs tweeking their calling conventions.")

(defvar *analyze-package-specs*
  '((set-ora-package                . (package-spec))
    (collect-package-spec-variables . (decl-var))
    (collect-funs-with-out+return   . (decl-fun)))
  "Analyze package specifications and prepare dynamic bindings.")

(defvar *convert-from-oracle*
  '((convert-data-type            . (cname))
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
  (loop :for (ora . pg) :in '(("SYSDATE" . "CURRENT_TIMESTAMP"))
     :collect (cons (make-qname :name ora) (make-qname :name pg))))

(defun add-oracle-constants (hash-table)
  "Add oracle constants to given HASH-TABLE."
  (loop :for (name . value) :in *oracle-constants*
     :do (setf (gethash name hash-table) value)))

(defun plsql-to-plpgsql (body list-of-package-specs)
  "Convert raw parsetree to a PL/pgSQL compatible parse tree."
  (let* ((*current-ora-package*  nil)   ; limit scope of changes
         (*current-function*     nil)   ; to there dynamic bindings
         (*packages-vars*        (make-hash-table :test 'equalp))
         (*funs-with-out+return* (make-hash-table :test 'equalp)))

    ;; analyze package specifications and prepare context for converting the
    ;; stored procedures and function...
    (loop :for spec :in list-of-package-specs
       :do (walk-apply spec *analyze-package-specs*))

    ;; add the oracle constants to the variables to transform
    (loop :for (name . value) :in *oracle-constants*
       :do (setf (gethash name *packages-vars*) value))

    ;; now convert Oracle PL code to something that PostgreSQL might accept
    (walk-apply body *convert-from-oracle*)

    (values *packages-vars* *funs-with-out+return* ;; body
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

      (proc   (setf (proc-name parsetree)
                    (make-qname :schema current-schema
                                :package current-package
                                :name (proc-name parsetree)))))))

#|
https://github.com/keithf4/oracle_fdw_snapshot/blob/master/ofdw.create_oracle_fdw_table.sql

CASE WHEN data_type = 'VARCHAR' THEN 'text' 
        WHEN data_type = 'VARCHAR2' THEN 'text'
        WHEN data_type = 'NVARCHAR2' THEN 'text'  
        WHEN data_type = 'CHAR' THEN 'char'||'('||data_length||')'
        WHEN data_type = 'NCHAR' THEN 'char'||'('||data_length||')'
        WHEN data_type = 'CLOB' THEN 'text'
        WHEN data_type = 'CFILE' THEN 'text'
        WHEN data_type = 'RAW' THEN 'bytea'
        WHEN data_type = 'BLOB' THEN 'bytea'
        WHEN data_type = 'BFILE' THEN 'bytea'
        WHEN data_type = 'NUMBER' THEN 'numeric'
        WHEN data_type = 'FLOAT' THEN 'float8'
        WHEN data_type = 'BINARY_FLOAT' THEN 'float8'
        WHEN data_type = 'DATE' THEN 'timestamp'
        WHEN data_type = 'TIMESTAMP' THEN 'timestamp'
        WHEN data_type = 'TIMESTAMP WITH TIME ZONE' THEN 'timestamptz'
        WHEN data_type = 'INTERVAL YEAR TO MONTH' THEN 'interval'
        WHEN data_type = 'INTERVAL DAY TO SECOND' THEN 'interval'
        WHEN data_type = 'LONG' THEN 'text'
        WHEN data_type = 'LONG RAW' THEN 'bytea'
        ELSE 'WTF->' || data_type END
|#

(defvar *oracle-data-type-mapping*
  (alexandria:alist-hash-table '(("VARCHAR"                  . "text")
                                 ("VARCHAR2"                 . "text")
                                 ("NVARCHAR2"                . "text")
                                 ("CHAR"                     . "text")
                                 ("NCHAR"                    . "text")
                                 ("CLOB"                     . "text")
                                 ("CFILE"                    . "text")
                                 ("RAW"                      . "bytea")
                                 ("BLOB"                     . "bytea")
                                 ("BFILE"                    . "bytea")
                                 ("NUMBER"                   . "numeric")
                                 ("FLOAT"                    . "float")
                                 ("BINARY_FLOAT"             . "float8")
                                 ("DATE"                     . "timestamptz")
                                 ("TIMESTAMP"                . "timestamp")
                                 ("TIMESTAMP WITH TIME ZONE" . "timestamptz")
                                 ("INTERVAL YEAR TO MONTH"   . "interval")
                                 ("INTERVAL DAY TO SECOND"   . "interval")
                                 ("LONG"                     . "text")
                                 ("LONG RAW"                 . "bytea"))
                               :test 'equal))

(defun convert-data-type (cname)
  "Map given Oracle data type CNAME into a PostgreSQL suitable data type."
  (when (and (null (cname-schema cname))
             (null (cname-relname cname)))

    (let ((data-type (cname-attribute cname)))
      (setf (cname-attribute cname)
            (gethash data-type *oracle-data-type-mapping* data-type)))))

;;;
;;; Fix output types of functions signatures and call sites.
;;;
(defun collect-funs-with-out+return (decl-fun)
  "Collect names of function with both out and return usage so that we can
   later process (and properly transform) their call sites."
  (when (decl-fun-ret-type decl-fun)
    (let ((out-params (loop :for param :in (decl-fun-arg-list decl-fun)
                         :when (member (funarg-mode param) '(:out :inout))
                         :collect param)))
      (when out-params
        ;; remember the name of the function so that we can later rewrite
        ;; its funcall sites.
        ;;
        ;; first fully qualify function name in the declaration
        (setf (decl-fun-name decl-fun)
              (make-qname :schema (qname-package *current-ora-package*)
                          :package (qname-name *current-ora-package*)
                          :name (decl-fun-name decl-fun)))

        (setf (gethash (decl-fun-name decl-fun) *funs-with-out+return*)
              decl-fun)))))

(defun out+return-to-returns-record (fun)
  "In Oracle it's possible to have both OUT parameters and a return value,
   in PostgreSQL the return value is composed of the OUT parameters."
  (when (gethash (fun-name fun) *funs-with-out+return*)
    ;; replace the hash-table entry with the full function for easier
    ;; manipulation later
    ;; (setf (gethash (fun-name fun) *funs-with-out+return*) fun)

    (let ((new-arg-name
           (make-qname :schema nil :package nil :name "__ret__"))
          (new-ret-type
           (make-data-type :cname (make-cname :schema nil
                                              :relname nil
                                              :attribute "record"))))

      ;; first append the return value as another parameter
      (appendf (fun-arg-list fun)
               (list (make-funarg :name new-arg-name
                                  :type (fun-ret-type fun)
                                  :mode :out)))

      ;; now change the return type to "record"
      (setf (fun-ret-type fun) new-ret-type))))

(defun out+return-call-sites (funcall)
  "We need to add a return variable and tweak the code to use it."
  (when (gethash (pl-funcall-name funcall) *funs-with-out+return*)
    (format t "out+return: call-site ~a~%"
            (typecase *current-function*
              (fun  (fun-name *current-function*))
              (proc (proc-name *current-function*))))

    (let ((callee (gethash (pl-funcall-name funcall) *funs-with-out+return*)))
      (format t "              funcall ~a~%" (pl-funcall-name funcall))
      (format t "             arg-list ~a~%" (decl-fun-arg-list callee))
      (format t "             ret-type ~a~%" (decl-fun-ret-type callee)))))

;;;
;;; Some Oracle funcalls needs to be converted into PERFORM nodes
;;;
(defun funcall-to-perform (parsetree)
  "Change stray funcalls into perform nodes."
  (flet ((replace-funcalls (list-of-nodes)
           (loop :for rest :on list-of-nodes :by #'cdr
              :when (typep (car rest) 'pl-funcall)
              :do (setf (car rest) (make-pl-perform :funcall (car rest))))))

    (typecase parsetree
      (code    (replace-funcalls (code-body parsetree)))

      (pl-if   (replace-funcalls (pl-if-then-body parsetree))
               (replace-funcalls (pl-if-elsif-list parsetree))
               (replace-funcalls (pl-if-else-body parsetree)))

      (pl-case (replace-funcalls (pl-case-else-body parsetree)))

      (pl-case-when (replace-funcalls (pl-case-when-body parsetree)))

      (pl-for     (replace-funcalls (pl-for-body parsetree)))
      (pl-forall  (replace-funcalls (pl-forall-body parsetree)))

      (pl-exception-when
       (replace-funcalls (pl-exception-when-body parsetree))))))

;;;
;;; Package Spec Variables
;;;
(defun collect-package-spec-variables (decl-var)
  "Return a list of package variables names."
  (setf (gethash (make-qname :schema (qname-package *current-ora-package*)
                             :package (qname-name *current-ora-package*)
                             :name (decl-var-name decl-var))
                 *packages-vars*)
        (decl-var-default decl-var)))

(defun process-package-vars (parsetree)
  "Package variable references are unqualified qname that are declared in
  *current-package-spec*, we need to do something about them."
  (flet ((package-var (qname)
           (when (qname-p qname)
             (let ((fqn                 ; fully qualified name
                    (cond
                      ((assoc qname *oracle-constants* :test #'equalp)
                       qname)
                      (t
                       (make-qname :schema (or (qname-schema qname)
                                               (qname-package *current-ora-package*))
                                   :package (or (qname-package qname)
                                                (qname-name *current-ora-package*))
                                   :name (qname-name qname))))))
               (gethash fqn *packages-vars*)))))

    (macrolet ((replace-package-var-ref (form)
                 (let ((value (gensym)))
                   `(let ((,value (package-var ,form)))
                      (when ,value
                        (setf ,form ,value))))))

      (typecase parsetree
        (pl-return   (replace-package-var-ref (pl-return-value parsetree)))
        (assignment  (replace-package-var-ref (assignment-value parsetree)))
        (pl-funcall  (loop :for rest :on (pl-funcall-arg-list parsetree)
                        :do (replace-package-var-ref (car rest))))))))
