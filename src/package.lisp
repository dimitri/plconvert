(defpackage #:plconvert.structs
  (:use #:cl)
  (:export #:qname
           #:make-qname
           #:qname-schema
           #:qname-package
           #:qname-name

           #:data-type
           #:make-data-type
           #:data-type-qname
           #:data-type-copy-from
           #:data-type-scale
           #:data-type-precision

           #:funarg
           #:make-funarg
           #:funarg-name
           #:funarg-type
           #:funarg-mode
           #:funarg-default

           #:decl-var
           #:make-decl-var
           #:decl-var-name
           #:decl-var-type
           #:decl-var-default

           #:decl-type
           #:make-decl-type
           #:decl-type-name
           #:decl-type-table

           #:code
           #:make-code
           #:code-decl-list
           #:code-body
           #:code-exception

           #:fun
           #:make-fun
           #:fun-name
           #:fun-arg-list
           #:fun-ret-type
           #:fun-code

           #:proc
           #:make-proc
           #:proc-name
           #:proc-arg-list
           #:proc-code))

(defpackage #:plconvert.parser
  (:use #:cl #:esrap #:plconvert.structs)
  (:export #:parse-package-body))

(defpackage #:plconvert
  (:use #:cl #:plconvert.parser #:plconvert.structs)
  (:import-from #:alexandria
                #:read-file-into-string
                #:read-file-into-byte-vector)
  (:import-from #:split-sequence
                #:split-sequence)
  (:export #:*user*
           #:*pass*
           #:send-function))
