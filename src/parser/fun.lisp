;;;
;;; Oracle PL/SQL quick parser
;;;

(in-package #:plconvert.parser)

(defrule function-block (and declarations
                             kw-begin body
                             (? exception)
                             kw-end (? maybe-qualified-namestring)
                             sc)
  (:lambda (x)
    (destructuring-bind (dec beg body except end fname sc) x
      (declare (ignore beg end fname sc))
      (make-code :decl-list dec
                 :body body
                 :exception except))))

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
                        close-cursor
                        funexpr-collection-call-method
                        funcall-no-args
                        execute)))

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
                       typename
                       (? (and (or := kw-default) expression))
                       ignore-whitespace)
  (:lambda (params)
    (destructuring-bind (ws1 name ws2 mode ws3 type default ws4) params
      (declare (ignore ws1 ws2 ws3 ws4))
      (let ((mode (or mode :in)))
        (make-funarg :name name
                     :type type
                     :mode mode
                     :default (when default (cdr default)))))))

(defrule in-out (and kw-in kw-out (? kw-nocopy)) (:constant :inout))

(defrule block (and (? declare) kw-begin body (? exception) kw-end sc)
  (:lambda (x)
    (destructuring-bind (dec beg body except end sc) x
      (declare (ignore beg end sc))
      (make-code :decl-list dec :body body :exception except))))

(defrule := (and ignore-whitespace ":=" ignore-whitespace) (:constant ':=))

(defrule assignment (and ignore-whitespace
                         (or funexpr-collection-call-method
                             funexpr-apply
                             funexpr-dot-accessor
                             funexpr
                             var)
                         :=
                         (or funexpr-collection-call-method
                             expression)
                         sc)
  (:lambda (assign)
    (destructuring-bind (ws1 varname eq rhs sc) assign
      (declare (ignore ws1 eq sc))
      (make-assignment :name varname :value rhs))))

(defrule tcl (and (or kw-commit kw-rollback) sc)
  (:lambda (tcl)
    (destructuring-bind (order sc) tcl
      (declare (ignore sc))
      (make-tcl :command order))))

(defrule control-block (or block-loop-fetch-into block-loop
                           block-if
                           block-for block-forall
                           block-case))

(defrule block-if (and kw-if expression
                       kw-then body
                       (* block-elsif)
                       (? (and kw-else body))
                       kw-end kw-if
                       sc)
  (:lambda (block-if)
    (destructuring-bind (i1 e th then elsif else end i2 sc) block-if
      (declare (ignore i1 th end i2 sc))
      (make-pl-if :cond e
                  :then-body then
                  :elsif-list elsif
                  :else-body (cadr else)))))

(defrule block-elsif (and kw-elsif expression kw-then body)
  (:destructure (elsif expr then body) (declare (ignore elsif then))
                (make-pl-elsif :cond expr :body body)))

(defrule block-for (and kw-for namestring
                        kw-in (or funexpr (or subquery query) for-range namestring)
                        kw-loop
                        body
                        kw-end kw-loop
                        sc)
  (:lambda (block-for)
    (destructuring-bind (for var in set loop body e l sc) block-for
      (declare (ignore for in loop e l sc))
      (make-pl-for :var var :set set :body body))))

(defrule dot-dot (and ignore-whitespace ".." ignore-whitespace) (:constant '|..|))

(defrule for-range (and expression dot-dot expression)
  (:destructure (start to end) (declare (ignore to))
                (make-pl-for-range :start start :end end)))

(defrule block-forall (and kw-forall namestring kw-in for-range query)
  (:destructure (forall var in range query)
                (declare (ignore forall in))
                (make-pl-forall :var var :set range :body query)))

(defrule block-loop-fetch-into
    (and kw-loop kw-fetch namestring kw-into namestring sc
         body
         kw-end kw-loop sc)
  (:destructure (lp fetch cursor into var sc body end lp2 sc2)
                (declare (ignore lp fetch into sc end lp2 sc2))
                (make-pl-loop :cursor cursor :var var :body body)))

(defrule block-loop (and kw-loop body kw-end kw-loop sc)
  (:destructure (lp body end lp2 sc)
                (declare (ignore lp end lp2 sc))
                (make-pl-loop :body body)))

(defrule continue (and kw-continue (? (and kw-when expression)) sc)
  (:destructure (c w sc) (declare (ignore c sc))
                (make-pl-continue :cond (cdr w))))

(defrule block-case (and kw-case (? (and (! kw-when) expression))
                         (+ case-when)
                         (? (and kw-else (or body statement)))
                         kw-end kw-case
                         sc)
  (:lambda (block-case)
    (destructuring-bind (c1 e1 when else end c2 sc) block-case
      (declare (ignore c1 end c2 sc))
      (make-pl-case :expr e1 :when-list when :else-body (cadr else)))))

(defrule case-when (and kw-when expression kw-then (or body statement))
  (:destructure (w expr then body)
                (declare (ignore w then))
                (make-pl-case-when :cond expr :body body)))

(defrule open-cursor (and kw-open (or cursor-for-query cursor-funcall))
  (:destructure (o cursor) (declare (ignore o)) cursor))

(defrule cursor-funcall funcall
  (:lambda (x) (make-pl-open :funcall x)))

(defrule cursor-for-query (and namestring kw-for (or query query-name))
  (:destructure (name for q)
                (declare (ignore for))
                (make-pl-open :name name :query q)))

(defrule query-name (and namestring sc)
  (:destructure (name sc) (declare (ignore sc)) name))

(defrule fetch-cursor (and kw-fetch maybe-qualified-namestring
                           (? (and kw-bulk kw-collect))
                           kw-into fetch-target-list
                           sc)
  (:lambda (x)
    (destructuring-bind (fetch name bulk-collect into var sc) x
      (declare (ignore fetch into sc))
      (make-pl-fetch :qname var :expr name :bulk (not (null bulk-collect))))))

(defrule fetch-target-list (and maybe-qualified-namestring
                                (* another-maybe-qualified-namestring))
  (:destructure (first rest) (list* first rest)))

(defrule another-maybe-qualified-namestring (and ignore-whitespace
                                                 ","
                                                 ignore-whitespace
                                                 maybe-qualified-namestring)
  (:destructure (ws1 c ws2 name) (declare (ignore c ws1 ws2)) name))

(defrule close-cursor (and kw-close maybe-qualified-namestring sc)
  (:destructure (close name sc)
                (declare (ignore close sc))
                (make-pl-close :qname name)))

(defrule exception (and kw-exception (+ when-exception))
  (:destructure (exception when-list)
                (declare (ignore exception))
                (make-pl-exception :when-list when-list)))

(defrule when-exception (and kw-when maybe-qualified-namestring
                             kw-then (+ exception-body))
  (:lambda (when-exception)
    (destructuring-bind (w cond then body) when-exception
      (declare (ignore w then))
      (make-pl-exception-when :cond cond :body body))))

(defrule exception-body (or tcl funcall assignment return control-block null query))

(defrule null (and kw-null sc) (:constant :null))

(defrule execute (and kw-execute (? kw-immediate) expression sc)
  (:destructure (execute immediate-p sql sc)
                (declare (ignore execute sc))
                (make-pl-execute :sql sql :immediate-p immediate-p)))

(defrule return (and kw-return (? expression) sc)
  (:destructure (ret retval sc)
                (declare (ignore ret sc))
                (make-pl-return :value retval)))

(defrule raise (and kw-raise maybe-qualified-namestring sc)
  (:destructure (raise name sc)
                (declare (ignore raise sc))
                (make-pl-raise :exception name)))

(defrule exit (and kw-exit (? exit-when) sc)
  (:destructure (exit when sc)
                (declare (ignore exit sc))
                (make-pl-exit :when when)))

(defrule exit-when (and kw-when expression)
  (:destructure (w expr) (declare (ignore w)) expr))

;;; basically we ignore RESULT_CACHE
(defrule result-cache (and kw-result_cache (? relies-on)))
(defrule relies-on    (and kw-relies_on #\( namestring-list #\))
  (:destructure (kw open list close) (declare (ignore kw open close)) list))
(defrule namestring-list (and namestring (? (and "," namestring-list)))
  (:destructure (first rest) (cons first (rest rest))))
