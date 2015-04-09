;;;
;;; Oracle PL/SQL quick parser
;;;

(in-package #:plconvert.parser)

(defrule package-body (and create-or-replace-package-body
                           maybe-qualified-namestring
                           kw-as
                           (+ (or function procedure))
                           kw-end maybe-qualified-namestring
                           #\;
                           ignore-whitespace
                           (? (and "/" ignore-whitespace)))
  (:lambda (package)
    (destructuring-bind (c-o-r name as funs end n sc ws sl) package
      (declare (ignore c-o-r as end n sc ws sl))
      (make-package-body :qname name :object-list funs))))

(defrule procedure (and kw-procedure
                        namestring
                        fdef-arglist
                        kw-is
                        function-block)
  (:lambda (function)
    (destructuring-bind (p name args is fun)
        function
      (declare (ignore p is))
      (make-proc :name name
                 :arg-list args
                 :code fun))))

(defrule function (and kw-function
                       namestring
                       fdef-arglist
                       kw-return
                       typename
                       (? result-cache)
                       kw-is
                       function-block)
  (:lambda (function)
    (destructuring-bind (f name args r rettype result-cache is fun)
        function
      (declare (ignore f r result-cache is))
      (make-fun :name name
                :arg-list args
                :ret-type rettype
                :code fun))))

(defrule create-or-replace (and kw-create (? (and kw-or kw-replace)))
  (:constant :c-o-r))

(defrule create-or-replace-package-body (and create-or-replace kw-package kw-body)
  (:constant :c-o-r-pbody))
