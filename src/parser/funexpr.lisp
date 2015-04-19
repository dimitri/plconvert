;;;
;;; Oracle PL/SQL quick parser: function expressions
;;;

(in-package #:plconvert.parser)

(defrule fcall-arglist (and "(" (? fcall-args) ")")
  (:destructure (open args close) (declare (ignore open close)) args))

(defrule fcall-args (and fcall-arg (* another-fcall-arg))
  (:destructure (arg1 args) (list* arg1 args)))

(defrule another-fcall-arg (and "," fcall-arg)
  (:destructure (c arg) (declare (ignore c)) arg))

(defrule fcall-arg (or fcall-named-arg fcall-arg-expr))

(defrule fcall-arg-expr expression)

(defrule => "=>" (:constant '=>))

(defrule fcall-named-arg (and expression => expression)
  (:destructure (arg => name) (declare (ignore =>))
                (make-assignment :name arg :value name)))

(defrule funexpr (and ignore-whitespace
                      maybe-qualified-namestring
                      ignore-whitespace
                      fcall-arglist)
  (:lambda (funcall)
    (destructuring-bind (ws1 fname ws2 args) funcall
      (declare (ignore ws1 ws2))
      (make-pl-funcall :name fname :arg-list args))))

(defrule funcall (and funexpr ignore-whitespace sc)
  (:destructure (fun ws sc) (declare (ignore ws sc)) fun))

(defrule funcall-no-args (and ignore-whitespace
                              (! (or kw-if kw-else kw-end kw-for))
                              (or maybe-qualified-namestring)
                              ignore-whitespace sc)
  (:destructure (ws1 kw name ws2 sc)
                (declare (ignore kw ws1 ws2 sc))
                (make-pl-funcall :name name :arg-list nil)))

(defrule funexpr-apply (and funexpr fcall-arglist)
  (:lambda (x)
    (destructuring-bind (fun arglist) x
      (make-expr-op :operator "apply" :operands (list fun arglist)))))

(defrule funexpr-collection-call-method
    (and (or funexpr namestring) collection-call-method)
  (:destructure (collection call-method)
                (setf (expr-op-operands call-method)
                      (list* collection (expr-op-operands call-method)))
                call-method))
