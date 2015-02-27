(defpackage #:plconvert.structs
  (:use #:cl)
  (:export #:qname
           #:make-qname
           #:qname-schema
           #:qname-package
           #:qname-name

           #:cname
           #:make-cname
           #:cname-schema
           #:cname-relname
           #:cname-attribute

           #:data-type
           #:make-data-type
           #:data-type-cname
           #:data-type-copy-from
           #:data-type-scale
           #:data-type-precision

           #:funarg
           #:make-funarg
           #:funarg-name
           #:funarg-type
           #:funarg-mode
           #:funarg-default

           #:decl-fun
           #:make-decl-fun
           #:decl-fun-name
           #:decl-fun-arg-list
           #:decl-fun-ret-type

           #:decl-proc
           #:make-decl-proc
           #:decl-proc-name
           #:decl-proc-arg-list

           #:decl-var
           #:make-decl-var
           #:decl-var-name
           #:decl-var-type
           #:decl-var-default

           #:decl-type-table
           #:make-decl-type-table
           #:decl-type-table-name
           #:decl-type-table-table
           #:decl-type-table-index-by

           #:decl-type-cursor
           #:make-decl-type-cursor
           #:decl-type-cursor-name

           #:decl-type-record
           #:make-decl-type-record
           #:decl-type-record-name

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
           #:proc-code

           #:package-body
           #:make-package-body
           #:package-body-qname
           #:package-body-object-list

           #:package-spec
           #:make-package-spec
           #:package-spec-qname
           #:package-spec-decl-list

           #:assignment
           #:make-assignment
           #:assignment-name
           #:assignment-value

           #:tcl
           #:make-tcl
           #:tcl-command

           #:pl-if
           #:make-pl-if
           #:pl-if-cond
           #:pl-if-then-body
           #:pl-if-else-body

           #:pl-elsif
           #:make-pl-elsif
           #:pl-elsif-cond
           #:pl-elsif-body

           #:pl-for
           #:make-pl-for
           #:pl-for-var
           #:pl-for-set
           #:pl-for-body

           #:pl-forall
           #:make-pl-forall
           #:pl-forall-var
           #:pl-forall-set
           #:pl-forall-body

           #:pl-for-range
           #:make-pl-for-range
           #:pl-for-range-start
           #:pl-for-range-end

           #:pl-case
           #:make-pl-case
           #:pl-case-expr
           #:pl-case-when-list
           #:pl-case-else-body

           #:pl-case-when
           #:make-pl-case-when
           #:pl-case-cond
           #:pl-case-body

           #:pl-continue
           #:make-pl-continue
           #:pl-continue-cond

           #:pl-funcall
           #:make-pl-funcall
           #:pl-funcall-name
           #:pl-funcall-arg-list

           #:pl-open
           #:make-pl-open
           #:pl-open-name
           #:pl-open-query
           #:pl-open-funcall

           #:pl-fetch
           #:make-pl-fetch
           #:pl-fetch-qname
           #:pl-fetch-expr

           #:pl-close
           #:make-pl-close
           #:pl-close-qname

           #:pl-exception
           #:make-pl-exception
           #:pl-exception-when-list

           #:pl-exception-when
           #:make-pl-exception-when
           #:pl-exception-when-cond
           #:pl-exception-when-body

           #:pl-return
           #:make-pl-return
           #:pl-return-value

           #:pl-raise
           #:make-pl-raise
           #:pl-raise-exception

           #:query
           #:make-query
           #:query-sql

           #:expression
           #:make-expression
           #:expression-value

           ))

(defpackage #:plconvert.parser
  (:use #:cl #:esrap #:plconvert.structs)
  (:export #:parse-package-body
           #:parse-package-spec))

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
