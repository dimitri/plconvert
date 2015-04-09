(in-package #:plconvert)

(defvar *current-ora-package* nil
  "Defined when parsing a Package Body or a Package Specs node.")

(defvar *current-function* nil
  "Defined when parsing a function definition.")

(defvar *packages-vars* nil
  "Known packages level variables and values, in a hash table.")

(defvar *packages-funs* nil
  "Known packages function declarations, in a hash table.")

(defvar *funs-with-out+return* nil
  "List of functions that needs tweeking their calling conventions.")

