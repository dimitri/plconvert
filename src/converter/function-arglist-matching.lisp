;;;
;;; Graveyard for code that's been removed from out-return.lisp even before
;;; having had the chance to be commited to the repository. We keep it there
;;; for now in case we want to revive it to solve the (estimated) 1% problem
;;; of function-name/arity collisions and DTRT in call cases.
;;;

(in-package #:plconvert)

(defun make-proto-arg-type (data-type)
  "Simplify type declaration to what's needed to compare a funcall and a
   function prototype."
  (if (cname-relname (data-type-cname data-type))
      "text"
      (cname-attribute (data-type-cname data-type))))

(defun find-expr-type (expr)
  "Find expression type of given EXPR.

   EXP is either an expression or a variable name look-up the variable in
   local function declaration, then function prototype, then package
   variables."
  (typecase expr
    (qname      (or (let ((var (member (qname-name expr)
                                       (code-decl-list
                                        (etypecase *current-function*
                                          (fun  (fun-code *current-function*))
                                          (proc (proc-code *current-function*))))
                                       :key (lambda (decl)
                                              (when (and decl (decl-var-p decl))
                                                (decl-var-name decl)))
                                       :test #'string=)))
                      (when var
                        (let ((data-type (decl-var-type (first var))))
                          (make-proto-arg-type data-type))))

                    (let ((var (member (qname-name expr)
                                       (etypecase *current-function*
                                         (fun (fun-arg-list *current-function*))
                                         (proc (proc-arg-list *current-function*)))
                                       :key #'funarg-name
                                       :test #'string=)))
                      (when var
                        (let ((data-type (funarg-type (first var))))
                          (make-proto-arg-type data-type))))

                    (let ((var (gethash (fqn expr) *packages-vars*)))
                      (when var
                        (make-proto-arg-type (decl-var-type var))))))

    (pl-funcall (let ((fun (gethash (make-prototype expr) *packages-funs*)))
                  (when fun
                    (let ((data-type (decl-fun-ret-type fun)))
                      (make-data-type :cname (data-type-cname data-type))))))

    (expr-op    (when (string= (expr-op-operator expr) "||")
                  "text"))

    (string    "text")

    (expr-case nil)

    (assignment (find-expr-type (assignment-value expr)))))

(defun make-prototype (node)
  "Make a simplified prototype so that we can match against a funcall."
  (flet ((proto-arglist (funarg-list)
           ;; polymorphism is based on args types, that's it
           (loop :for funarg :in funarg-list
              :when (eq :in (funarg-mode funarg))
              :collect (make-proto-arg-type (funarg-type funarg))))

         (find-arg-types (funcall-params)
           (mapcar #'find-expr-type funcall-params)))

    (typecase node
      (decl-fun   (list* (decl-fun-name node)
                         (proto-arglist (decl-fun-arg-list node))))

      (fun        (list* (fun-name node)
                         (proto-arglist (fun-arg-list node))))

      (pl-funcall (list* (pl-funcall-name node)
                         (find-arg-types
                          (pl-funcall-arg-list node)))))))

