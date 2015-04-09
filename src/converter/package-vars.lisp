;;;
;;; Package Spec Variables
;;;
(in-package #:plconvert)

(defun fqn (qname)
  "Return a new qname fully qualified name from qname, based on
  *current-ora-package*."
  (make-qname :schema (or (qname-schema qname)
                          (qname-package *current-ora-package*))
              :package (or (qname-package qname)
                           (qname-name *current-ora-package*))
              :name (qname-name qname)))

(defun collect-package-spec-variables (decl-var)
  "Return a list of package variables names."
  (setf (gethash (fqn (make-qname :name (decl-var-name decl-var)))
                 *packages-vars*)
        decl-var))

(defun process-package-vars (parsetree)
  "Package variable references are unqualified qname that are declared in
  *current-package-spec*, we need to do something about them."
  (flet ((package-var (qname)
           (when (qname-p qname)
             (let ((fqn                 ; fully qualified name
                    (cond
                      ((assoc qname *oracle-constants* :test #'equalp) qname)
                      (t (fqn qname)))))
               (gethash fqn *packages-vars*)))))

    (macrolet ((replace-package-var-ref (form)
                 (let ((value (gensym)))
                   `(let ((,value (package-var ,form)))
                      (when ,value
                        (setf ,form (decl-var-default ,value)))))))

      (typecase parsetree
        (pl-return   (replace-package-var-ref (pl-return-value parsetree)))
        (assignment  (replace-package-var-ref (assignment-value parsetree)))
        (pl-funcall  (loop :for rest :on (pl-funcall-arg-list parsetree)
                        :do (replace-package-var-ref (car rest))))))))
