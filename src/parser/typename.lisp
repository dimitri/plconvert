;;;
;;; Oracle PL/SQL quick parser
;;;

(in-package #:plconvert.parser)

(defrule typename (or typename-copied-from typename-typmod typename-simple))

(defrule typename-typmod (and column-name typmod)
  (:destructure (name typmod)
                (make-data-type :cname name
                                :scale (getf typmod :scale)
                                :precision (getf typmod :precision))))

(defrule typename-copied-from (and column-name (and "%" namestring))
  (:destructure (name template)
                (make-data-type :cname name
                                :copy-from (second template))))

(defrule typename-simple column-name
  (:lambda (x) (make-data-type :cname x)))

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
