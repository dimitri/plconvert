;;;
;;; Oracle PL/SQL quick parser
;;;

(in-package #:plconvert.parser)

(defun uncomment (query)
  "query contains :comment bits, get rid'of em"
  (remove :comment query))

(defrule query (and ignore-whitespace
                    (or (~ "with")
                        (~ "select")
                        (~ "insert")
                        (~ "update")
                        (~ "delete"))
                    (* (or comments (not ";")))
                    ";")
  (:destructure (ws c q sc)
                (declare (ignore ws))
                (make-query :sql (text c (uncomment q) sc))))
