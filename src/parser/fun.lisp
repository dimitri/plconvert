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
                       (? (and kw-default expression))
                       ignore-whitespace)
  (:lambda (params)
    (destructuring-bind (ws1 name ws2 mode ws3 type default ws4) params
      (declare (ignore ws1 ws2 ws3 ws4))
      (let ((mode (or mode :in)))
        (make-funarg :name name
                     :type type
                     :mode mode
                     :default (when default (cdr default)))))))

(defrule in-out (and kw-in kw-out) (:constant :inout))

(defrule block (and declare kw-begin body exception kw-end sc)
  (:lambda (x)
    (destructuring-bind (dec beg body except end sc) x
      (declare (ignore beg end sc))
      (make-code :decl-list dec :body body :exception except))))

(defrule function-block (and declarations
                             kw-begin body
                             (? exception)
                             kw-end maybe-qualified-namestring
                             sc)
  (:lambda (x)
    (destructuring-bind (dec beg body except end fname sc) x
      (declare (ignore beg end fname sc))
      (make-code :decl-list dec
                 :body body
                 :exception except))))

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
                              (? default-value)
                              sc)
  (:lambda (x)
    (destructuring-bind (ws1 noise varname ws2 type default sc) x
      (declare (ignore noise ws1 ws2 sc))
      (make-decl-var :name varname :type type :default default))))

(defrule declare-varname (or dollar-varname varname-%option namestring))

(defrule default-value (and ignore-whitespace ":=" ignore-whitespace expression)
  (:destructure (ws1 a ws2 e) (declare (ignore a ws1 ws2)) e))

(defrule type-declaration (or type-table-of
                              type-ref-cursor
                              type-record))

(defrule type-table-of (and kw-type namestring
                            kw-is kw-table kw-of
                            type-definition
                            sc)
  (:lambda (x)
    (destructuring-bind (typ name is table of table-name sc) x
      (declare (ignore typ is table of sc))
      (cond ((and (consp table-name) (eq :hash (first table-name)))
             (make-decl-type-table :name name
                                   :table (getf (cdr table-name) :key)
                                   :index-by (getf (cdr table-name) :value)))

            (t                          ; var
             (make-decl-type-table :name name :table table-name))))))

(defrule type-ref-cursor (and kw-type namestring kw-is kw-ref kw-cursor sc)
  (:destructure (typ name is ref cursor sc)
                (declare (ignore typ is ref cursor sc))
                (make-decl-type-cursor :name name)))

(defrule type-definition (or index-by var))

(defrule index-by (and typename kw-index kw-by typename)
  (:destructure (type-of-value i b type-of-key)
                (declare (ignore i b))
                `(:hash :key ,type-of-key :value ,type-of-value)))

(defrule type-record (and kw-type namestring kw-is kw-record
                          type-record-attribute-list
                          sc)
  (:destructure (type name is rec att-list sc)
                (declare (ignore type is rec sc))
                (make-decl-type-record :name name :att-list att-list)))

(defrule type-record-attribute-list (and o-p (+ type-record-attribute) c-p)
  (:destructure (o att-list c) (declare (ignore o c)) att-list))

(defrule type-record-attribute (and namestring
                                    ignore-whitespace
                                    typename
                                    (? comma))
  (:lambda (x)
    (destructuring-bind (name ws type c) x
      (declare (ignore ws c))
      (make-decl-var :name name :type type))))

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

(defrule := (and ignore-whitespace ":=" ignore-whitespace) (:constant ':=))

(defrule assignment (and ignore-whitespace
                         (or funexpr-dot-accessor funexpr var)
                         :=
                         expression
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

(defrule control-block (or block-if block-for block-forall block-case))

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
                  :else-body (cdr else)))))

(defrule block-elsif (and kw-elsif expression kw-then body)
  (:destructure (elsif expr then body) (declare (ignore elsif then))
                (make-pl-elsif :cond expr :body body)))

(defrule block-for (and kw-for namestring kw-in (or funexpr query for-range)
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

(defrule continue (and kw-continue (? (and kw-when expression)) sc)
  (:destructure (c w sc) (declare (ignore c sc))
                (make-pl-continue :cond (cdr w))))

(defrule block-case (and kw-case (? expression)
                         (+ case-when)
                         (? (and kw-else (or body statement)))
                         kw-end kw-case
                         sc)
  (:lambda (block-case)
    (destructuring-bind (c1 e1 when else end c2 sc) block-case
      (declare (ignore c1 end c2 sc))
      (make-pl-case :expr e1 :when-list when :else-body (cdr else)))))

(defrule case-when (and kw-when expression kw-then (or body statement))
  (:destructure (w expr then body)
                (declare (ignore w then))
                (make-pl-case-when :cond expr :body body)))

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

(defrule open-cursor (and kw-open (or cursor-for-query cursor-funcall))
  (:destructure (o cursor) (declare (ignore o)) cursor))

(defrule cursor-funcall funcall
  (:lambda (x) (make-pl-open :funcall x)))

(defrule cursor-for-query (and namestring kw-for query)
  (:destructure (name for q)
                (declare (ignore for))
                (make-pl-open :name name :query q)))

(defrule fetch-cursor (and kw-fetch maybe-qualified-namestring
                           kw-bulk kw-collect kw-into maybe-qualified-namestring
                           sc)
  (:lambda (x)
    (destructuring-bind (fetch name bulk collect into var sc) x
      (declare (ignore fetch bulk collect into sc))
      (make-pl-fetch :qname var :expr name))))

(defrule close-cursor (and kw-close maybe-qualified-namestring sc)
  (:destructure (close name sc)
                (declare (ignore close sc))
                (make-pl-close :qname name)))

(defrule exception (and kw-exception (+ when-exception))
  (:destructure (exception when-list)
                (declare (ignore exception))
                (make-pl-exception :when-list when-list)))

(defrule when-exception (and kw-when maybe-qualified-namestring
                             kw-then exception-body)
  (:lambda (when-exception)
    (destructuring-bind (w cond then body) when-exception
      (declare (ignore w then))
      (make-pl-exception-when :cond cond :body body))))

(defrule exception-body (+ (or statement tcl funcall
                               assignment return control-block)))

(defrule return (and kw-return expression sc)
  (:destructure (ret retval sc)
                (declare (ignore ret sc))
                (make-pl-return :value retval)))

(defrule raise (and kw-raise maybe-qualified-namestring sc)
  (:destructure (raise name sc)
                (declare (ignore raise sc))
                (make-pl-raise :exception name)))

(defrule exit (and kw-exit sc) (:constant :exit))

