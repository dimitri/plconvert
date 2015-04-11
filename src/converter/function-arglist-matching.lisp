;;;
;;; When we have function-name / arity collisions, and when that collision
;;; hapens between out+return function and "normal" functions, then we need
;;; to find which implementation of the function is actually being called by
;;; deriving a full prototype of the function call, from its arguments.
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
                        (make-proto-arg-type (decl-var-type var))))

                    ;; Oracle Constant have already been translated at this
                    ;; point, so we need to match against their PostgreSQL
                    ;; spellings.
                    (loop :for (name . var) :in *oracle-constants*
                       :when (string-equal (decl-var-name var) (qname-name expr))
                       :return (make-proto-arg-type (decl-var-type var)))))

    (pl-funcall (let ((fun (gethash (make-prototype expr) *packages-funs*)))
                  (when fun
                    (let ((data-type (decl-fun-ret-type fun)))
                      (make-data-type :cname (data-type-cname data-type))))))

    (expr-op    (when (string= (expr-op-operator expr) "||")
                  "text"))

    (string    "text")
    (number    "numeric")

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

(defun print-prototype (prototype)
  "Pretty print a function's PROTOTYPE as computed with make-prototype."
  (format nil "~a(~{~a~^, ~})" (qname-to-string (first prototype)) (rest prototype)))

(defun print-in-out-arg-list (decl-fun)
  "For debugging purposes, pring all DECL-FUN arguments modes and type names."
  (format nil "~a(~{~a ~a~^, ~})"
          (qname-to-string (decl-fun-name decl-fun))
          (apply #'append
                 (mapcar (lambda (funarg)
                           (list (funarg-mode funarg)
                                 (cname-attribute
                                  (data-type-cname (funarg-type funarg)))))
                         (decl-fun-arg-list decl-fun)))))
