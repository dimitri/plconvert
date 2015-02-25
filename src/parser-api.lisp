;;;
;;; Oracle PL/SQL quick parser
;;;

(in-package #:plconvert.parser)

(defun parse-package-body (package-body-string)
  "Parse PACKAGE-BODY-STRING as an Oracle PACKAGE BODY statement."
  (parse 'package-body package-body-string))
