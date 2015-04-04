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

(defun collect-package-vars (directory)
  "Find all spec files in given directory and parse them to collect their
   variable definitions."
  (let ((vars (make-hash-table :test 'equalp)))
   (loop :for spec-file :in (find-spec-files directory)
      :for package-spec := (parse-file spec-file)
      :do (collect-package-spec-variables package-spec vars))

   ;; add some Oracle provided constants
   (add-oracle-constants vars)

   ;; and return our hash-table of Oracle package global variables
   ;; replacements.
   vars))

(defun convert-package (pathname packages-vars)
  "Convert a source file into a PL/pgSQL file."
  (let ((*current-package-vars* packages-vars))
    (plsql-to-plpgsql (parse-file pathname))))
