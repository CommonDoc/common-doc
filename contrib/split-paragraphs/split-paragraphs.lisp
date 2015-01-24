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

(defun excise-paragraph-separators (list)
  "Take a list of strings or other elements. Separate strings by paragraphs,
leaving a paragraph marker between each string."
  (let ((output (list)))
    (loop for elem in list do
      (if (stringp elem)
          ;; If it's a string, split it by its separator
          (let ((split (split-paragraph elem)))
            (if (stringp split)
                ;; Just a regular string with no separators, add it to the
                ;; output
                (push split output)
                ;; A list of text nodes, add each to the output, followed by a
                ;; paragraph separator marker
                (loop for text in split do
                  (push text output)
                  (push +paragraph-marker+ output))))
          ;; If it's another node, add it to the output unconditionally
          (push elem output)))
    output))
