(in-package :cl-user)
(defpackage common-doc.split-paragraphs
  (:use :cl)
  (:documentation "Main package of split-paragraphs."))
(in-package :common-doc.split-paragraphs)

(defparameter +paragraph-separator-regex+
  (ppcre:create-scanner "\\n\\n")
  "A regular expression that matches double newlines.")

(defparameter +paragraph-marker+
  :paragraph-split
  "A marker that is inserted between strings after they are separated by
paragraphs.")

(defun has-paragraph-separator (string)
  "Does string contain a paragraph separator?"
  (if (ppcre:scan +paragraph-separator-regex+ string) t))

(defun split-paragraph (string)
  "Split a string by the separator into a list of strings, or return the intact
string if it has none."
  (if (has-paragraph-separator string)
      (ppcre:split "\\n\\n" string)
      string))
