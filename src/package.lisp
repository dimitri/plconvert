(defpackage #:plconvert.structs
  (:use #:cl)
  (:export #:qname
           #:qname-p
           #:make-qname
           #:qname-schema
           #:qname-package
           #:qname-name

           #:qname-to-string

           #:cname
           #:cname-p
           #:make-cname
           #:cname-schema
           #:cname-relname
           #:cname-attribute

           #:data-type
           #:data-type-p
           #:make-data-type
           #:data-type-cname
           #:data-type-copy-from
           #:data-type-scale
           #:data-type-precision

           #:funarg
           #:funarg-p
           #:make-funarg
           #:funarg-name
           #:funarg-type
           #:funarg-mode
           #:funarg-default

           #:decl-fun
           #:decl-fun-p
           #:make-decl-fun
           #:decl-fun-name
           #:decl-fun-arg-list
           #:decl-fun-ret-type

           #:decl-proc
           #:decl-proc-p
           #:make-decl-proc
           #:decl-proc-name
           #:decl-proc-arg-list

           #:decl-var
           #:decl-var-p
           #:make-decl-var
           #:decl-var-name
           #:decl-var-type
           #:decl-var-default

           #:decl-type-table
           #:decl-type-table-p
           #:make-decl-type-table
           #:decl-type-table-name
           #:decl-type-table-table
           #:decl-type-table-index-by

           #:decl-type-cursor
           #:decl-type-cursor-p
           #:make-decl-type-cursor
           #:decl-type-cursor-name

           #:decl-type-record
           #:decl-type-record-p
           #:make-decl-type-record
           #:decl-type-record-name
           #:decl-type-record-att-list

           #:code
           #:code-p
           #:make-code
           #:code-decl-list
           #:code-body
           #:code-exception

           #:fun
           #:fun-p
           #:make-fun
           #:fun-name
           #:fun-arg-list
           #:fun-ret-type
           #:fun-code

           #:proc
           #:proc-p
           #:make-proc
           #:proc-name
           #:proc-arg-list
           #:proc-code

           #:package-body
           #:package-body-p
           #:make-package-body
           #:package-body-qname
           #:package-body-object-list

           #:package-spec
           #:package-spec-p
           #:make-package-spec
           #:package-spec-qname
           #:package-spec-decl-list

           #:assignment
           #:assignment-p
           #:make-assignment
           #:assignment-name
           #:assignment-value

           #:tcl
           #:tcl-p
           #:make-tcl
           #:tcl-command

           #:pl-if
           #:pl-if-p
           #:make-pl-if
           #:pl-if-cond
           #:pl-if-then-body
           #:pl-if-elsif-list
           #:pl-if-else-body

           #:pl-elsif
           #:pl-elsif-p
           #:make-pl-elsif
           #:pl-elsif-cond
           #:pl-elsif-body

           #:pl-for
           #:pl-for-p
           #:make-pl-for
           #:pl-for-var
           #:pl-for-set
           #:pl-for-body

           #:pl-forall
           #:pl-forall-p
           #:make-pl-forall
           #:pl-forall-var
           #:pl-forall-set
           #:pl-forall-body

           #:pl-for-range
           #:pl-for-range-p
           #:make-pl-for-range
           #:pl-for-range-start
           #:pl-for-range-end

           #:pl-case
           #:pl-case-p
           #:make-pl-case
           #:pl-case-expr
           #:pl-case-when-list
           #:pl-case-else-body

           #:pl-case-when
           #:pl-case-when-p
           #:make-pl-case-when
           #:pl-case-when-cond
           #:pl-case-when-body

           #:pl-continue
           #:pl-continue-p
           #:make-pl-continue
           #:pl-continue-cond

           #:pl-funcall
           #:pl-funcall-p
           #:make-pl-funcall
           #:pl-funcall-name
           #:pl-funcall-arg-list

           #:pl-perform
           #:pl-perform-p
           #:make-pl-perform
           #:pl-perform-name
           #:pl-perform-arg-list

           #:pl-open
           #:pl-open-p
           #:make-pl-open
           #:pl-open-name
           #:pl-open-query
           #:pl-open-funcall

           #:pl-fetch
           #:pl-fetch-p
           #:make-pl-fetch
           #:pl-fetch-qname
           #:pl-fetch-expr

           #:pl-close
           #:pl-close-p
           #:make-pl-close
           #:pl-close-qname

           #:pl-exception
           #:pl-exception-p
           #:make-pl-exception
           #:pl-exception-when-list

           #:pl-exception-when
           #:pl-exception-when-p
           #:make-pl-exception-when
           #:pl-exception-when-cond
           #:pl-exception-when-body

           #:pl-return
           #:pl-return-p
           #:make-pl-return
           #:pl-return-value

           #:pl-raise
           #:pl-raise-p
           #:make-pl-raise
           #:pl-raise-exception

           #:query
           #:query-p
           #:make-query
           #:query-sql

           #:expr-op
           #:expr-op-p
           #:make-expr-op
           #:expr-op-operator
           #:expr-op-operands

           #:expr-case
           #:expr-case-p
           #:make-expr-case
           #:expr-case-when-list
           #:expr-case-else-expr

           #:expr-case-when
           #:expr-case-when-p
           #:make-expr-case-when
           #:expr-case-when-cond
           #:expr-case-when-expr

           #:comment
           #:comment-p
           #:make-comment
           #:comment-text
           #:comment-single-line-p

           ))

(defpackage #:plconvert.parser
  (:use #:cl #:esrap #:plconvert.structs)
  (:export #:parse-package-body
           #:parse-package-spec))

(defpackage #:plconvert
  (:use #:cl #:plconvert.parser #:plconvert.structs)
  (:import-from #:alexandria
                #:appendf
                #:read-file-into-string
                #:read-file-into-byte-vector
                #:alist-hash-table)
  (:import-from #:split-sequence
                #:split-sequence)
  (:export #:*user*
           #:*pass*
           #:send-function))
