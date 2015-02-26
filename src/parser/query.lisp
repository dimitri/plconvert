;;;
;;; Oracle PL/SQL quick parser
;;;

(in-package #:plconvert.parser)

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
                (list :query (text c (uncomment q) sc))))
