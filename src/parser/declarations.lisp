;;;
;;; Oracle PL/SQL quick parser
;;;

(in-package #:plconvert.parser)

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
      (let ((name (make-cname :schema nil :relname nil :attribute name)))
        (cond ((and (consp table-name) (eq :hash (first table-name)))
               (make-decl-type-table :name name
                                     :table (getf (cdr table-name) :key)
                                     :index-by (getf (cdr table-name) :value)))

              (t                        ; var
               (make-decl-type-table :name name
                                     :table table-name
                                     :index-by nil)))))))

(defrule type-ref-cursor (and kw-type namestring kw-is kw-ref kw-cursor sc)
  (:destructure (typ name is ref cursor sc)
                (declare (ignore typ is ref cursor sc))
                (make-decl-type-cursor :name (make-cname :schema nil
                                                         :relname nil
                                                         :attribute name))))

(defrule type-definition (or index-by typename))

(defrule index-by (and typename kw-index kw-by typename)
  (:destructure (type-of-value i b type-of-key)
                (declare (ignore i b))
                `(:hash :key ,type-of-key :value ,type-of-value)))

(defrule type-record (and kw-type namestring kw-is kw-record
                          type-record-attribute-list
                          sc)
  (:destructure (type name is rec att-list sc)
                (declare (ignore type is rec sc))
                (make-decl-type-record :name (make-cname :schema nil
                                                         :relname nil
                                                         :attribute name)
                                       :att-list att-list)))

(defrule type-record-attribute-list (and o-p (+ type-record-attribute) c-p)
  (:destructure (o att-list c) (declare (ignore o c)) att-list))

(defrule type-record-attribute (and namestring
                                    ignore-whitespace
                                    typename
                                    (? comma))
  (:lambda (x)
    (destructuring-bind (name ws type c) x
      (declare (ignore ws c))
      (make-decl-var :name (make-cname :schema nil
                                       :relname name
                                       :attribute name)
                     :type type))))
