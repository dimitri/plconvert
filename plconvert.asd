;;;; plconvert.asd

(asdf:defsystem #:plconvert
    :serial t
    :description "Convert from Stored Procedure PL/SQL to PLpgSQL"
    :author "Dimitri Fontaine <dimitri@2ndQuadrant.fr>"
    :license "The PostgreSQL Licence"
    :depends-on (#:uiop              ; host system integration
		 #:esrap             ; parser generator
                 #:split-sequence    ; split strings
                 #:cl-ppcre          ; Regular Expressions
                 #:alexandria        ; Some utilities
                 #:daemon            ; run the repo server in the background
                 #:drakma            ; HTTP client, to check server status
		 )
    :components
    ((:module src
              :components
              ((:file "package")
               (:file "structs" :depends-on ("package"))

               (:module utils
                        :depends-on ("package")
                        :components ((:file "cli-parser")))

               (:module parser
                        :depends-on ("package" "structs")
                        :components ((:file "keywords")
                                     (:file "misc")
                                     (:file "typename" :depends-on ("keywords"
                                                                    "misc"))
                                     (:file "expr" :depends-on ("keywords"
                                                                "misc"))
                                     (:file "query" :depends-on ("keywords"))

                                     (:file "fun" :depends-on ("keywords"
                                                               "misc"
                                                               "query"
                                                               "typename"))
                                     (:file "pspec" :depends-on ("keywords"
                                                                 "misc"
                                                                 "expr"
                                                                 "query"
                                                                 "fun"))
                                     (:file "pbody" :depends-on ("keywords"
                                                                 "misc"
                                                                 "expr"
                                                                 "query"
                                                                 "fun"))))

               (:file "parser-api" :depends-on ("package" "utils" "parser"))

               (:file "plconvert" :depends-on ("package"
                                               "utils"
                                               "parser"
                                               "parser-api"))))))

