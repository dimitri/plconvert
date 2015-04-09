;;;
;;; Some Oracle funcalls needs to be converted into PERFORM nodes
;;;
(in-package #:plconvert)

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
