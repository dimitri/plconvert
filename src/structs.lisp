;;;
;;; Oracle PL/SQL quick parser
;;;

(in-package #:plconvert.structs)

;;; This package exports structs to use from within the parser, which is
;;; supposed to be producing them.
;;;
;;; The parser output should be a tree of structs that the code generator
;;; walks in order to print PLpgSQL code back.

(defstruct qname schema package name)

(defstruct data-type qname copy-from scale precision)

(defstruct funarg name type mode default)

(defstruct decl-var name type default)
(defstruct decl-type name table index-by)

(defstruct code decl-list body exception)

(defstruct fun name arg-list ret-type code)

(defstruct proc name arg-list code)

(defstruct package-body qname object-list)

(defstruct assignment name value)

(defstruct tcl command)

(defstruct pl-if cond then-body elsif-list else-body)
(defstruct pl-elsif cond body)

(defstruct pl-for var set body)
(defstruct pl-forall var set body)
(defstruct pl-for-range start end)

(defstruct pl-case expr when-list else-body)
(defstruct pl-case-when cond body)

(defstruct pl-continue cond)

(defstruct pl-funcall name arg-list)

(defstruct pl-open name funcall query)
(defstruct pl-fetch qname expr)
(defstruct pl-close qname)

(defstruct pl-exception when-list)
(defstruct pl-exception-when cond body)

(defstruct pl-return value)
(defstruct pl-raise exception)

(defstruct query sql)

;;;
;;; Some basic printing for the debugging
;;;
(defun qname-to-string (qname)
  (with-slots (schema package name) qname
    (format nil "~@[~a.~]~@[~a.~]~a" schema package name)))

(defmethod print-object ((qname qname) stream)
  (print-unreadable-object (qname stream :type t :identity t)
    (format stream "~a" (qname-to-string qname))))

(defun data-type-to-string (data-type)
  (with-slots (qname copy-from scale precision) data-type
    (format nil "~a~@[%~a~]~@[(~a~@[,~a~])~]"
            (qname-to-string qname) copy-from scale precision)))

(defmethod print-object ((data-type data-type) stream)
  (print-unreadable-object (data-type stream :type t :identity t)
    (format stream "~a" (data-type-to-string data-type))))

(defmethod print-object ((funarg funarg) stream)
  (print-unreadable-object (funarg stream :type t :identity t)
    (with-slots (name type mode default) funarg
      (format stream "~@[~a~] ~a ~a~@[DEFAULT ~s~]"
              (string-upcase mode)
              name
              (data-type-to-string type)
              default))))

(defmethod print-object ((decl-var decl-var) stream)
  (print-unreadable-object (decl-var stream :type t :identity t)
    (with-slots (name type default) decl-var
      (format stream "~a ~a~@[ := ~a~]"
              name (data-type-to-string type) default))))

(defmethod print-object ((decl-type decl-type) stream)
  (print-unreadable-object (decl-type stream :type t :identity t)
    (with-slots (name table index-by) decl-type
      (format stream "~a IS TABLE OF ~a~@[ INDEX BY ~a~]"
              name (qname-to-string table) index-by))))
