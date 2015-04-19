;;;
;;; Oracle PL/SQL quick parser
;;;

(in-package #:plconvert.parser)

(defrule typename (or typename-copied-from typename-typmod typename-simple))

(defrule typename-typmod (and bare-typename typmod)
  (:destructure (name typmod)
                (make-data-type :cname name
                                :scale (getf typmod :scale)
                                :precision (getf typmod :precision))))

(defrule typename-copied-from (and bare-typename (and "%" namestring))
  (:destructure (name template)
                (make-data-type :cname name
                                :copy-from (second template))))

(defrule typename-simple bare-typename
  (:lambda (x) (make-data-type :cname x)))

(defrule typmod (and ignore-whitespace "("
                     ignore-whitespace dec-number ignore-whitespace
                     (? (and "," ignore-whitespace dec-number ignore-whitespace))
                     ignore-whitespace ")")
  (:lambda (x)
    (destructuring-bind (open ws1 ws2 scale ws3 precision ws4 close) x
      (declare (ignore open ws1 ws2 ws3 ws4 close))
      (let ((precision (when precision
                         (destructuring-bind (c ws1 precision ws2) precision
                           (declare (ignore c ws1 ws2))
                           precision))))
        (list :scale scale :precision precision)))))

(defrule bare-typename (or timestamptz column-name))
(defrule timestamptz (and kw-timestamp kw-with kw-time kw-zone)
  (:constant (make-cname :attribute "timestamptz")))
