;;;
;;; Oracle PL/SQL quick parser
;;;

(in-package #:plconvert.parser)

(defun uncomment (query)
  "query contains :comment bits, get rid'of em"
  (remove :comment query))

(defrule sql (and (or (~ "with")
                      (~ "select")
                      (~ "insert")
                      (~ "update")
                      (~ "delete"))
                  (* (or comments (not ";"))))
  (:destructure (c q) (make-query :sql (text c (uncomment q)))))

(defrule query (and ignore-whitespace sql ";")
  (:destructure (ws sql sc) (declare (ignore ws sc)) sql))

(defrule subquery (and o-p sql c-p)
  (:destructure (o sql c) (declare (ignore o c)) sql))
