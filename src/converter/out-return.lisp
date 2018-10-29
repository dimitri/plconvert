;;;
;;; Given the parser output (an AST using internal reprensentation nodes as
;;; defined in src/structs.lisp), prepare the AST to be output as a PL/pgSQL
;;; function.

(in-package #:plconvert)

;;;
;;; Fix output types of functions signatures and call sites.
;;;
(defun make-signature (node)
  "Make a function signature to quickly associate call sites and
   definitions. As we have data-type based polyphormism in PL languages, the
   signature is going to be (cons function-name arity) here.

   The function hash-table then maintains a list of complete IN argument
   data type list to enable precise matching in case more than one single
   name/arity function exists."
  (flet ((arity (funarg-list)
           (loop :for funarg :in funarg-list
              :count (eq :in (funarg-mode funarg))))

         (find-arg-types (funcall-params)
           (mapcar #'find-expr-type funcall-params)))

    (typecase node
      (decl-fun   (cons (qname-to-string (decl-fun-name node))
                         (arity (decl-fun-arg-list node))))

      (fun        (cons (qname-to-string (fun-name node))
                        (arity (fun-arg-list node))))

      (pl-funcall (cons (qname-to-string (pl-funcall-name node))
                        (length (pl-funcall-arg-list node)))))))

(defun print-signature (signature)
  (format nil "~a/~a" (car signature) (cdr signature)))

(defun collect-package-spec-funs (decl-fun &optional (fun-table *packages-funs*))
  "Collect all functions declarations."
  (let* ((signature (make-signature decl-fun))
         (decl-list (gethash signature fun-table)))
    (push decl-fun decl-list)
    (setf (gethash signature fun-table) decl-list)))

(defun fun-with-out+return-p (node)
  "Predicate that is non-nil when NODE (either a DECL-FUN or a FUN instance)
  represents a function that both has OUT parameters and RETURNS another
  value."
  (let ((arg-list (typecase node
                    (fun      (fun-arg-list node))
                    (decl-fun (decl-fun-arg-list node))))

        (ret-type (typecase node
                    (fun      (fun-ret-type node))
                    (decl-fun (decl-fun-ret-type node)))))

   (when ret-type
     (loop :for param :in arg-list
        :thereis (member (funarg-mode param) '(:out :inout))))))

(defun collect-funs-with-out+return (decl-fun)
  "Collect names of function with both out and return usage so that we can
   later process (and properly transform) their call sites."
  (when (fun-with-out+return-p decl-fun)
    ;; (format t "out+return collect: ~a~%" (print-signature
    ;;                                       (make-signature decl-fun)))
    (collect-package-spec-funs decl-fun *funs-with-out+return*)))

(defun out+return-to-returns-record (fun)
  "In Oracle it's possible to have both OUT parameters and a return value,
   in PostgreSQL the return value is composed of the OUT parameters."
  (when (fun-with-out+return-p fun)
    ;; (format t "out+return  edit: ~a~%" (print-signature (make-signature fun)))
    (let ((new-ret-type
           (make-data-type :cname (make-cname :schema nil
                                              :relname nil
                                              :attribute "record"))))

      ;; first append the return value as another parameter
      (appendf (fun-arg-list fun)
               (list (make-funarg :name "__ret__"
                                  :type (fun-ret-type fun)
                                  :mode :out)))

      ;; now change the return type to "record"
      (setf (fun-ret-type fun) new-ret-type))))

(defun out+return-call-sites (funcall)
  "We need to add a return variable and tweak the code to use it."
  (let ((signature (make-signature funcall)))
    (when (gethash signature *funs-with-out+return*)
      ;;
      ;; So we are calling a function withan out+return signature... it
      ;; could well be that this function also has overloading definitions
      ;; without out+return.
      ;;
      ;; In that case, we need to guestimate which variant we are actually
      ;; calling, if possible (we need to be able to derive the data type of
      ;; all the parameters at the call site).
      ;;
      (let ((candidates (gethash signature *packages-funs*)))
        (flet ((print-debug-info (action)
                 (format t "out+return: call-site ~a~%"
                         (typecase *current-function*
                           (fun  (fun-name *current-function*))
                           (proc (proc-name *current-function*))))

                 (format t "            traversed ~{~a~^ ~}~%"
                         (mapcar
                          (lambda (node)
                            (print-unreadable-object (node t :type t :identity t)))
                          *traversed-nodes*))

                 (format t "              funcall ~a~%"
                         (print-signature signature))
                 (format t "                 args ~a~%"
                         (pl-funcall-arg-list funcall))
                 (format t "            prototype ~a~%"
                         (print-prototype (make-prototype funcall)))

                 (if (= 1 (length candidates))
                     (format t "              calling ~a~%"
                             (print-in-out-arg-list (first candidates)))
                     (format t "           candidates ~@<~a~@:>~%"
                             (mapcar #'print-in-out-arg-list candidates)))

                 (format t "      ~a~%" action)))

          (cond
            ;; if we have more than one candidate function definition being
            ;; called here, try to find the one we're calling.
            ((< 1 (length candidates))
             ;; let's try to find which function we're calling here exactly
             (let ((match (first (member (make-prototype funcall)
                                         candidates
                                         :key #'make-prototype
                                         :test #'equalp))))

               (cond
                 ((and match (fun-with-out+return-p match))
                  ;; Replace the call
                  (print-debug-info "TODO: replace"))

                 (match
                  ;; full debug mode needs this
                  (print-debug-info "done"))

                 ((not match)
                  ;; TODO: add a WARNING in the source code
                  (print-debug-info "FAIL: no match!")))))

            ;; We have only one function definition candidate, and it's an
            ;; out+return function, just replace it
            ((and (= 1 (length candidates))
                  (fun-with-out+return-p (first candidates)))
             (print-debug-info "TODO: replace"))))))))

