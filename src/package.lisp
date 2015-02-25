(defpackage #:plconvert.parser
  (:use #:cl #:esrap)
  (:export #:parse-package-body))

(defpackage #:plconvert
  (:use #:cl #:plconvert.parser)
  (:import-from #:alexandria
                #:read-file-into-string
                #:read-file-into-byte-vector)
  (:import-from #:split-sequence
                #:split-sequence)
  (:export #:*user*
           #:*pass*
           #:send-function))
