;;;
;;; Oracle PL/SQL quick parser
;;;

(in-package #:plconvert.parser)

(defrule fdef-arglist (and ignore-whitespace "(" fdef-args ")")
  (:destructure (ws open args close) (declare (ignore ws open close)) args))

(defrule fdef-args (and fdef-arg (* another-fdef-arg))
  (:destructure (arg1 args) (list* arg1 args)))

(defrule another-fdef-arg (and "," fdef-arg)
  (:destructure (c arg) (declare (ignore c)) arg))

(defrule fdef-arg (and ignore-whitespace
                       namestring
                       ignore-whitespace
                       (? (or in-out kw-in kw-out))
                       ignore-whitespace
                       (? typename)
                       (? (and kw-default expr))
                       ignore-whitespace)
  (:lambda (params)
    (destructuring-bind (ws1 name ws2 mode ws3 type default ws4) params
      (declare (ignore ws1 ws2 ws3 ws4))
      (let ((mode (or mode :in)))
        (list :name name
              :type type
              :mode mode
              :default (when default (cdr default)))))))

(defrule in-out (and kw-in kw-out) (:constant :inout))

(defrule typename (and maybe-qualified-namestring
                       (? (and "%" namestring))
                       (? typmod))
  (:destructure (name template typmod)
                (list :type name
                      :template (when template (second template))
                      :scale (when typmod (getf typmod :scale))
                      :precision (when typmod (getf typmod :precision)))))

(defrule typmod (and "("
                     ignore-whitespace dec-number ignore-whitespace
                     (? (and "," ignore-whitespace dec-number ignore-whitespace))
                     ")")
  (:lambda (x)
    (destructuring-bind (open ws1 scale ws2 precision close) x
      (declare (ignore open ws1 ws2 close))
      (let ((precision (when precision
                         (destructuring-bind (c ws1 precision ws2) precision
                           (declare (ignore c ws1 ws2))
                           precision))))
        (list :scale scale :precision precision)))))

(defrule block (and declare kw-begin body exception kw-end ";")
  (:lambda (x)
    (destructuring-bind (dec beg body except end sc) x
      (declare (ignore beg end sc))
      (list :block (list :declarations dec :body body :exception except)))))

(defrule function-block (and declarations
                             kw-begin body
                             (? exception)
                             kw-end maybe-qualified-namestring
                             ignore-whitespace
                             ";")
  (:lambda (x)
    (destructuring-bind (dec beg body except end fname ws sc) x
      (declare (ignore beg end fname ws sc))
      (list :declarations dec :body body :exception except))))

(defrule declare (and kw-declare declarations)
  (:destructure (dec vars) (declare (ignore dec)) `(:declare ,@vars)))

(defrule declarations (* (or cursor var-declaration type-declaration)))

(defrule cursor (and kw-cursor namestring fdef-arglist kw-is query)
  (:destructure (c name args is q)
                (declare (ignore c is))
                `(:cursor ,name ,args ,q)))

(defrule var-declaration (and ignore-whitespace (! kw-begin)
                              declare-varname
                              ignore-whitespace
                              typename
                              ignore-whitespace
                              (? default-value)
                              ignore-whitespace ";")
  (:lambda (x)
    (destructuring-bind (ws1 noise varname ws2 type ws3 default ws4 sc) x
      (declare (ignore noise ws1 ws2 ws3 ws4 sc))
      (list :var varname :type type :default default))))

(defrule declare-varname (or dollar-varname varname-%option namestring))

(defrule default-value (and ":=" ignore-whitespace expr)
  (:destructure (a ws e) (declare (ignore a ws)) e))

(defrule type-declaration (and kw-type namestring
                               kw-is kw-table kw-of
                               type-definition
                               ignore-whitespace
                               ";")
  (:destructure (typ name is table of table-name ws sc)
                (declare (ignore typ is table of ws sc))
                `(:type ,name :table ,table-name)))

(defrule type-definition (or index-by var))

(defrule index-by (and typename kw-index kw-by typename)
  (:destructure (type-of-value i b type-of-key)
                (declare (ignore i b))
                `(:hash :key ,type-of-key :value ,type-of-value)))

(defrule body    (+ (or block
                        assignment
                        control-block
                        continue
                        exit
                        funcall
                        query
                        tcl
                        return
                        raise
                        open-cursor
                        fetch-cursor
                        close-cursor)))

(defrule assignment (and ignore-whitespace
                         (or funexpr-dot-accessor funexpr var)
                         ignore-whitespace ":=" ignore-whitespace
                         expr
                         ignore-whitespace ";")
  (:lambda (assign)
    (destructuring-bind (ws1 varname ws2 eq ws3 rhs ws4 sc) assign
      (declare (ignore eq ws1 ws2 ws3 ws4 sc))
      (list :assign (list varname rhs)))))

(defrule tcl (and (or kw-commit kw-rollback) ";")
  (:lambda (tcl)
    (destructuring-bind (order sc) tcl
      (declare (ignore sc))
      order)))

(defrule control-block (or block-if block-for block-forall block-case))

(defrule block-if (and kw-if expr
                       kw-then body
                       (* block-elsif)
                       (? (and kw-else body))
                       kw-end kw-if
                       ";")
  (:lambda (block-if)
    (destructuring-bind (i1 e th then elsif else end i2 sc) block-if
      (declare (ignore i1 th end i2 sc))
      `(:if ,e (:then ,then) ,@elsif ,@(when else (cdr else))))))

(defrule block-elsif (and kw-elsif expr kw-then body)
  (:destructure (elsif expr then body) (declare (ignore elsif then))
                `(:elsif ,expr ,body)))

(defrule block-for (and kw-for namestring kw-in (or funexpr query for-range)
                        kw-loop
                        body
                        kw-end kw-loop
                        ";")
  (:lambda (block-for)
    (destructuring-bind (for var in set loop body e l sc) block-for
      (declare (ignore for in loop e l sc))
      `(:for ,var :in ,set :loop ,body))))

(defrule for-range (and expr ignore-whitespace ".." ignore-whitespace expr)
  (:destructure (begin ws1 to ws2 end) (declare (ignore to ws1 ws2))
                (list :range begin end)))

(defrule block-forall (and kw-forall namestring kw-in for-range query)
  (:destructure (forall var in range query)
                (declare (ignore forall in))
                `(:forall ,var :in ,range :query ,query)))

(defrule continue (and kw-continue (? (and kw-when expr)) ";")
  (:destructure (c w sc) (declare (ignore c sc))
                `(:continue ,@ (when w (list :when (cdr w))))))

(defrule block-case (and kw-case (? expr)
                         (+ case-when)
                         (? (and kw-else (or body statement)))
                         kw-end kw-case
                         ";")
  (:lambda (block-case)
    (destructuring-bind (c1 e1 when else end c2 sc) block-case
      (declare (ignore c1 e1 end c2 sc))
      `(:case ,@when
         ,@(when else (list :else (cdr else)))))))

(defrule case-when (and kw-when expr kw-then (or body statement))
  (:destructure (w expr then body) (declare (ignore w then)) `(:when ,expr ,body)))

(defrule fcall-arglist (and "(" (? fcall-args) ")")
  (:destructure (open args close) (declare (ignore open close)) args))

(defrule fcall-args (and fcall-arg (* another-fcall-arg))
  (:destructure (arg1 args) (list* arg1 args)))

(defrule another-fcall-arg (and "," fcall-arg)
  (:destructure (c arg) (declare (ignore c)) arg))

(defrule fcall-arg (and ignore-whitespace expr ignore-whitespace)
  (:destructure (ws1 arg ws2) (declare (ignore ws1 ws2)) arg))

(defrule funexpr (and ignore-whitespace
                      maybe-qualified-namestring
                      ignore-whitespace
                      fcall-arglist)
  (:lambda (funcall)
    (destructuring-bind (ws1 fname ws2 args) funcall
      (declare (ignore ws1 ws2))
      (list :funcall fname args))))

(defrule funcall (and funexpr ignore-whitespace ";")
  (:destructure (fun ws sc) (declare (ignore ws sc)) fun))

(defun uncomment (query)
  "query contains :comment bits, get rid'of em"
  (remove :comment query))

(defrule open-cursor (and kw-open (or cursor-for-query funcall))
  (:destructure (o cursor) (declare (ignore o)) `(:open ,@cursor)))

(defrule cursor-for-query (and namestring kw-for query)
  (:destructure (name for q) (declare (ignore for)) (list name q)))

(defrule fetch-cursor (and kw-fetch maybe-qualified-namestring
                           kw-bulk kw-collect kw-into maybe-qualified-namestring
                           ";")
  (:lambda (x)
    (destructuring-bind (fetch expr bulk collect into var sc) x
      (declare (ignore fetch bulk collect into sc))
      `(:fetch :into ,var ,expr))))

(defrule close-cursor (and kw-close maybe-qualified-namestring ";")
  (:destructure (close name sc) (declare (ignore close sc)) `(:close ,name)))

(defrule exception (and kw-exception (+ when-exception)))

(defrule when-exception (and kw-when maybe-qualified-namestring
                             kw-then exception-body))

(defrule exception-body (+ (or statement tcl funcall
                               assignment return control-block)))

(defrule return (and kw-return expr ";")
  (:destructure (ret label sc) (declare (ignore ret sc)) (cons :return label)))

(defrule raise (and kw-raise maybe-qualified-namestring ";")
  (:destructure (raise name sc) (declare (ignore raise sc)) (cons :raise name)))

(defrule exit (and kw-exit ";") (:constant :exit))

