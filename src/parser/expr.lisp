;;;
;;; Oracle PL/SQL quick parser
;;;

(in-package #:plconvert.parser)

;;;
;;; Let's deal internally with lisp-like expressions
;;;
(defun simplify (form)
  "Given an expression such as a + b + c, the parser returns the lisp parse
   tree (a (+ b (+ c))) and we want (+ a b c) of course."
  (destructuring-bind (expression nested-expression) form
    (cond
      ;; a
      ((null nested-expression) expression)

      ;; a + b
      ((and (not (consp expression))
            (= 1 (length nested-expression)))
       (destructuring-bind (op primary) (first nested-expression)
         (list op expression primary)))

      ;; general case
      ;; a + b + c, a + b - c, a + c or c - d and d < f
      (t;; (not (consp expression))
       (let* ((first-expr    (first nested-expression))
              (first-op      (first first-expr))
              (first-primary (second first-expr))
              (result        (list first-op expression first-primary)))
         (reduce (lambda (res expr)
                   (destructuring-bind (op primary-or-exp) expr
                     (if (eq op (first res))
                         (append res (list primary-or-exp))
                         (list* op res (list primary-or-exp)))))
                 (rest nested-expression)
                 :initial-value result))))))

(defrule +  (and ignore-whitespace "+" ignore-whitespace)  (:constant '+))
(defrule -  (and ignore-whitespace "-" ignore-whitespace)  (:constant '-))
(defrule *  (and ignore-whitespace "*" ignore-whitespace)  (:constant '*))
(defrule /  (and ignore-whitespace "/" ignore-whitespace)  (:constant '/))
(defrule %  (and ignore-whitespace "%" ignore-whitespace)  (:constant '%))
(defrule <  (and ignore-whitespace "<" ignore-whitespace)  (:constant '<))
(defrule <= (and ignore-whitespace "<=" ignore-whitespace) (:constant '<=))
(defrule >  (and ignore-whitespace ">" ignore-whitespace)  (:constant '>))
(defrule >= (and ignore-whitespace ">=" ignore-whitespace) (:constant '>=))
(defrule =  (and ignore-whitespace "=" ignore-whitespace)  (:constant '=))
(defrule <> (and ignore-whitespace (or "!=" "<>") ignore-whitespace) (:constant '<>))

(defrule ~~ (and ignore-whitespace (~ "like") ignore-whitespace)  (:constant '~~))
(defrule !~~ (and ignore-whitespace (~ "not like") ignore-whitespace)  (:constant '!~~))

(defrule op-and (and ignore-whitespace (~ "and") ignore-whitespace) (:constant 'and))
(defrule op-or  (and ignore-whitespace (~ "or")  ignore-whitespace)  (:constant 'or))
(defrule op-not (and ignore-whitespace (~ "not") ignore-whitespace)  (:constant 'not))

(defrule ¦¦ (and ignore-whitespace "||" ignore-whitespace) (:constant '¦¦))

(defrule o (and ignore-whitespace "(" ignore-whitespace)  (:constant nil))
(defrule c (and ignore-whitespace ")" ignore-whitespace)  (:constant nil))

(defrule expr    (and bool (* (or exp-and exp-or))) (:function simplify))
(defrule exp-and (and op-and bool))
(defrule exp-or  (and op-or bool))

(defrule bool    (and comp (* (or lt le gt ge)))    (:function simplify))
(defrule lt      (and < comp))
(defrule le      (and <= comp))
(defrule gt      (and > comp))
(defrule ge      (and >= comp))

(defrule comp     (and factor (* (or add sub
                                     eql neq
                                     like
                                     not-like)))  (:function simplify))
(defrule add      (and + factor))
(defrule sub      (and - factor))
(defrule eql      (and = factor))
(defrule neq      (and <> factor))
(defrule like     (and ~~ factor))
(defrule not-like (and !~~ factor))

(defrule factor  (and concat (* (or mul div mod)))  (:function simplify))
(defrule mul     (and * concat))
(defrule div     (and / concat))
(defrule mod     (and % concat))

(defrule concat  (and primary (* conc))             (:function simplify))
(defrule conc    (and ¦¦ primary))

(defrule primary (or parens case-expr term-is-null not-term term))

(defrule parens  (and o expr c)
  (:destructure (o e c) (declare (ignore o c)) (list e)))

(defrule not-term (and op-not primary))

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

(defrule statement (and expr ignore-whitespace ";")
  (:destructure (e ws sc) (declare (ignore ws sc)) e))

(defrule expression expr (:lambda (expr) (make-expression :value expr)))
