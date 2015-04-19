;;;
;;; Oracle PL/SQL quick parser: collection call methods
;;;
;;; http://docs.oracle.com/cd/B19306_01/appdev.102/b14261/collection_method.htm
;;;

(in-package #:plconvert.parser)

(defrule dot (and ignore-whitespace "." ignore-whitespace) (:constant '|.|))

(defrule collection-call-method (and dot (or collection-noarg
                                             collection-one-arg
                                             collection-arglist))
  (:destructure (dot method) (declare (ignore dot)) method))

(defrule collection-noarg (or kw-count kw-first kw-last kw-limit)
  (:lambda (op)
    (make-expr-op :operator op)))

(defrule collection-one-arg (and (or kw-next kw-prior kw-exists kw-trim)
                                 o-p
                                 expression
                                 c-p)
  (:destructure (op o index c)
                (declare (ignore o c))
                (make-expr-op :operator op :operands (list index))))

(defrule collection-arglist (and (or kw-extend kw-delete)
                                 o-p
                                 expression
                                 (* another-expression)
                                 c-p)
  (:destructure (op o first rest c)
                (declare (ignore o c))
                (make-expr-op :operator op :operands (list* first rest))))

(defrule another-expression (and "," expression)
  (:destructure (coma e) (declare (ignore coma)) e))

