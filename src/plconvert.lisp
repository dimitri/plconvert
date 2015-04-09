;;;
;;; Main API
;;;

(in-package #:plconvert)

(defvar *options* nil)
(defvar *external-format* :utf-8)

(defun file-type (pathname)
  "Determine what to parse depending on PATHNAME type."
  (cond ((string-equal "pkb" (pathname-type pathname))
         :package-body)

        ((string-equal "pks" (pathname-type pathname))
         :package-spec)

        (t :unknown)))

(defun parse-file (pathname &optional (external-format *external-format*))
  "Parse given filename."
  (ecase (file-type pathname)
    (:package-body
     (format t "Parsing package body from: ~s~%" pathname)
     (parse-package-body
      (read-file-into-string pathname :external-format external-format)))
    (:package-spec
     (format t "Parsing package spec from: ~s~%" pathname)
     (parse-package-spec
      (read-file-into-string pathname :external-format external-format)))))

(defun parse-directory-files (pathname)
  "Parse all files in a given directory."
  (mapcar #'parse-file (uiop:directory-files pathname)))

(defun check-file (pathname)
  (let ((ast (parse-file pathname)))
    (length (etypecase ast
              (package-body (package-body-object-list ast))
              (package-spec (package-spec-decl-list ast))))))

(defun check-directory-files (pathname)
  (let ((summary
         (mapcar (lambda (pathname)
                   (list (file-type pathname)
                         (pathname-name pathname)
                         (check-file pathname)))
                 (uiop:directory-files pathname))))
    (values summary (reduce #'+ (mapcar #'third summary)))))


;;;
;;; As we need to collect package-variable definitions for all the packages
;;; of a given Oracle database, the main API works with a directory tree for
;;; packages and schema wherein to find the .pkb and .pks files. We parse
;;; the whole set of .pks files then consider the .pkb.
;;;
(defun find-spec-files (directory)
  "Find all spec files in a given directory."
  (let (spec-files)
    (flet ((collect-spec-files (filename)
             (when (eq :package-spec (file-type filename))
               (push filename spec-files))))
      (uiop:collect-sub*directories directory
                                    t   ; always collect
                                    t   ; always recurse
                                    (lambda (directory)
                                      (mapcar #'collect-spec-files
                                              (uiop:directory-files directory)))))
    spec-files))

(defun convert-package (pathname directory)
  "Convert a source file into a PL/pgSQL file."
  (let ((list-of-package-specs
         (mapcar #'parse-file (find-spec-files directory))))
    (plsql-to-plpgsql (parse-file pathname) list-of-package-specs)))


(defun test (expression input directory)
  "Test parsing and converting INPUT given a DIRECTORY of specs etc."
  (let ((list-of-package-specs
         (mapcar #'parse-file (find-spec-files directory))))
    (plsql-to-plpgsql (plconvert.parser::parse expression input)
                      list-of-package-specs)))
