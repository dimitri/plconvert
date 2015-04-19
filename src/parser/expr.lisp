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
      ((and (not (expr-op-p expression))
            (= 1 (length nested-expression)))

       (let ((op (first nested-expression)))
         (setf (expr-op-operands op)
               (list* expression (expr-op-operands op)))
         op))

      ;; general case
      ;; a + b + c, a + b - c, a + c or c - d and d < f
      (t
       (let* ((result (first nested-expression)))

         (setf (expr-op-operands result)
               (list* expression (expr-op-operands result)))

         (reduce (lambda (res expr)
                   (if (and (expr-op-p expr)
                            (equal (expr-op-operator res)
                                   (expr-op-operator expr)))
                       ;; add one operand to the list
                       (progn
                         (setf (expr-op-operands res)
                               (append (expr-op-operands res)
                                       (expr-op-operands expr)))
                         result)
                       ;; not the same operator, so it's an operand to the
                       ;; first level
                       (progn
                         (setf (expr-op-operands expr)
                               (list* result (expr-op-operands expr)))
                         (setf result expr))))
                 (rest nested-expression)
                 :initial-value result)
         result)))))

(defun make-operator (form)
  (destructuring-bind (ws1 operator ws2) form
    (declare (ignore ws1 ws2))
    (make-expr-op :operator operator)))

(defun op-term (form)
  (destructuring-bind (op term) form
    (setf (expr-op-operands op) (list term))
    op))

(defrule ws ignore-whitespace)

(defrule +  (and ws "+" ws)  (:function make-operator))
(defrule -  (and ws "-" ws)  (:function make-operator))
(defrule *  (and ws "*" ws)  (:function make-operator))
(defrule /  (and ws "/" ws)  (:function make-operator))
(defrule %  (and ws "%" ws)  (:function make-operator))
(defrule <  (and ws "<" ws)  (:function make-operator))
(defrule <= (and ws "<=" ws) (:function make-operator))
(defrule >  (and ws ">" ws)  (:function make-operator))
(defrule >= (and ws ">=" ws) (:function make-operator))
(defrule =  (and ws "=" ws)  (:function make-operator))
(defrule <> (and ws (or "!=" "<>") ws) (:function make-operator))

(defrule ~~ (and ws (~ "like") ws)       (:function make-operator))
(defrule !~~ (and ws (~ "not like") ws)  (:function make-operator))

(defrule at-time-zone (and kw-at kw-time kw-zone)
  (:lambda (k)
    (declare (ignore k))
    (make-expr-op :operator "at time zone")))

(defrule op-in (and ws (~ "in") ws)         (:function make-operator))
(defrule op-not-in (and ws (~ "not in") ws) (:function make-operator))

(defrule op-and (and ws (~ "and") ws) (:function make-operator))
(defrule op-or  (and ws (~ "or")  ws) (:function make-operator))
(defrule op-not (and ws (~ "not") ws) (:function make-operator))

(defrule ¦¦ (and ws "||" ws) (:function make-operator))

(defrule expr    (and bool (* (or exp-and exp-or))) (:function simplify))
(defrule exp-and (and op-and bool) (:function op-term))
(defrule exp-or  (and op-or bool)  (:function op-term))

(defrule bool    (and time (* (or lt le gt ge)))    (:function simplify))
(defrule lt      (and < time)  (:function op-term))
(defrule le      (and <= time) (:function op-term))
(defrule gt      (and > time)  (:function op-term))
(defrule ge      (and >= time) (:function op-term))

(defrule time    (and comp (* atz))                (:function simplify))
(defrule atz     (and at-time-zone comp) (:function op-term))

(defrule comp     (and factor (* (or add sub
                                     eql neq
                                     like
                                     not-like)))  (:function simplify))
(defrule add      (and + factor)   (:function op-term))
(defrule sub      (and - factor)   (:function op-term))
(defrule eql      (and = factor)   (:function op-term))
(defrule neq      (and <> factor)  (:function op-term))
(defrule like     (and ~~ factor)  (:function op-term))
(defrule not-like (and !~~ factor) (:function op-term))

(defrule factor  (and concat (* (or mul div mod)))  (:function simplify))
(defrule mul     (and * concat)    (:function op-term))
(defrule div     (and / concat)    (:function op-term))
(defrule mod     (and % concat)    (:function op-term))

(defrule concat  (and primary (* (or conc in not-in)))   (:function simplify))
(defrule conc    (and ¦¦ primary)    (:function op-term))
(defrule in      (and op-in list-of-expr)     (:function op-term))
(defrule not-in  (and op-not-in list-of-expr) (:function op-term))

(defrule primary (or parens case-expr cast term-is-null not-term term))

(defrule parens  (and o-p expr c-p)
  (:destructure (o e c) (declare (ignore o c)) (list e)))

(defrule list-of-expr (and o-p expr (* another-expr) c-p)
  (:destructure (o first rest c) (declare (ignore o c)) (list* first rest)))

(defrule another-expr (and ws "," ws expr)
  (:destructure (ws1 c ws2 expr) (declare (ignore ws1 c ws2)) expr))

(defrule not-term (and op-not primary))

(defrule funexpr-dot-accessor (and funexpr "." namestring)
  (:lambda (x)
    (destructuring-bind (fun dot slotname) x
      (declare (ignore dot))
      (make-expr-op :operator "dot-accessor" :operands (list fun slotname)))))

(defrule term (or kw-null
                  funexpr-dot-accessor funexpr
                  quoted-text var dec-number))

(defrule term-is-null (and term kw-is (? kw-not) kw-null)
  (:destructure (term is not null)
                (declare (ignore is null))
                (make-expr-op :operator (format nil "is~@[ not~] null" not)
                              :operands (list term))))

(defrule case-expr (and kw-case
                        (? expr)
                        (+ case-expr-when)
                        (? (and kw-else expr))
                        kw-end)
  (:lambda (case-expr)
    (destructuring-bind (c1 expr when else end) case-expr
      (declare (ignore c1 end))
      (make-expr-case :expr expr
                      :when-list when
                      :else-expr (when else (cadr else))))))

(defrule case-expr-when (and kw-when expr kw-then expr)
  (:destructure (w w-expr then t-expr)
                (declare (ignore w then))
                (make-expr-case-when :cond w-expr :expr t-expr)))

(defrule cast (and kw-cast o-p expr kw-as typename c-p)
  (:destructure (cast o expr as type c)
                (declare (ignore cast o c as))
                (make-expr-cast :expr expr :as-type type)))

(defrule statement (and expr ignore-whitespace ";")
  (:destructure (e ws sc) (declare (ignore ws sc)) e))

(defrule expression (and ignore-whitespace expr ignore-whitespace)
  (:destructure (ws1 expr ws2) (declare (ignore ws1 ws2))
                expr))
