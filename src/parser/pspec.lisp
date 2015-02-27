;;;
;;; Oracle PL/SQL quick parser
;;;

(in-package #:plconvert.parser)

(defrule package-spec (and create-or-replace
                           kw-package
                           maybe-qualified-namestring
                           kw-as
                           (+ package-declaration)
                           kw-end maybe-qualified-namestring
                           sc
                           (? (and "/" ignore-whitespace)))
  (:lambda (package)
    (destructuring-bind (c-o-r package name as decl-list end n sc sl) package
      (declare (ignore c-o-r package as end n sc sl))
      (make-package-spec :qname name :decl-list decl-list))))

(defrule package-declaration (and (! kw-end)
                                  (or var-declaration
                                      const-declaration
                                      type-declaration
                                      proc-declaration
                                      fun-declaration
                                      cursor))
  (:destructure (end decl) (declare (ignore end)) decl))

(defrule proc-declaration (and kw-procedure
                               namestring
                               fdef-arglist
                               sc)
  (:lambda (fun-decl)
    (destructuring-bind (f name args sc) fun-decl
      (declare (ignore f sc))
      (make-decl-proc :name name :arg-list args))))

(defrule fun-declaration (and kw-function
                              namestring
                              fdef-arglist
                              kw-return
                              typename
                              sc)
  (:lambda (fun-decl)
    (destructuring-bind (f name args r rettype sc) fun-decl
      (declare (ignore f r sc))
      (make-decl-fun :name name :arg-list args :ret-type rettype))))


(defrule const-declaration (and ignore-whitespace
                                namestring
                                kw-constant
                                ignore-whitespace
                                typename
                                (? default-value)
                                sc)
  (:lambda (const)
    (destructuring-bind (ws1 varname c ws2 type default sc) const
      (declare (ignore c ws1 ws2 sc))
      (make-decl-var :name varname :type type :default default))))
