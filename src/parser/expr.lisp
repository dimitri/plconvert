;;;
;;; Oracle PL/SQL quick parser
;;;

(in-package #:plconvert.parser)

(defrule like (and (? kw-not) kw-like)
  (:destructure (n l) (declare (ignore l)) (if n "not like" "like")))

(defrule op (and ignore-whitespace
                 (or "=" "!=" "<>"
                     "||"
                     "+" "-" "*" "/"
                     "<=" ">="  "<" ">"
                     like)
                 ignore-whitespace)
  (:destructure (ws1 op ws2) (declare (ignore ws1 ws2)) op))

(defrule var  (or dollar-varname varname-%option maybe-qualified-namestring)
  (:lambda (x) (list :var x)))

(defrule funexpr-dot-accessor (and funexpr "." namestring)
  (:lambda (x)
    (destructuring-bind (fun dot slotname) x
      (declare (ignore dot))
      (list :expr (list :dot slotname fun)))))

(defrule term (or kw-null
                  funexpr-dot-accessor funexpr
                  quoted-text var dec-number))

(defrule term-is-null (and term kw-is (? kw-not) kw-null)
  (:destructure (term is not null)
                (declare (ignore is null))
                (list (if not :is-not-null :is-null) term)))

(defrule expr-op-expr (and expr op expr))

(defrule binary-op-expr (or expr-op-expr term-is-null))

(defrule expr-and-expr (and expr kw-and expr))
(defrule expr-or-expr  (and expr kw-or expr))
(defrule not-expr      (and kw-not expr)
  (:destructure (n e) (declare (ignore n)) e))

(defrule boolean-binary-expr (or expr-and-expr expr-or-expr)
  (:lambda (e)
    (destructuring-bind (e1 op e2) e
      ;; flatten sub-trees and consolidate members as in &rest lisp functions
      (let ((e1 (if (and (consp e1) (eq op (car e1))) (cdr e1) (list e1)))
            (e2 (if (and (consp e2) (eq op (car e2))) (cdr e2) (list e2))))
        `(,op ,@e1 ,@e2)))))

(defrule boolean-expr (or not-expr boolean-binary-expr))

(defrule case-expr (and kw-case
                        (+ case-expr-when)
                        (? (and kw-else expr))
                        kw-end)
  (:lambda (case-expr)
    (destructuring-bind (c1 when else end) case-expr
      (declare (ignore c1 end))
      `(:case ,@when
         ,@(when else (list :else (cdr else)))))))

(defrule case-expr-when (and kw-when expr kw-then expr)
  (:destructure (w w-expr then t-expr)
                (declare (ignore w then))
                `(:when ,w-expr ,t-expr)))

(defrule open-paren (and ignore-whitespace "(") (:constant nil))
(defrule close-paren (and ignore-whitespace ")") (:constant nil))
(defrule parens-expr (and open-paren expr close-paren)
  (:destructure (open e close) (declare (ignore open close)) e))

(defrule expr (or case-expr boolean-expr binary-op-expr parens-expr term)
  (:lambda (x) (cond ((and (consp x) (member (car x) '(:expr :and :or))) x)
                     (t (list :expr x)))))

(defrule statement (and expr ignore-whitespace ";")
  (:destructure (e ws sc) (declare (ignore ws sc)) e))

