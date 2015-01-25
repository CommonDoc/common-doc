(in-package :cl-user)
(defpackage common-doc.split-paragraphs
  (:use :cl)
  (:import-from :common-doc
                :text-node
                :paragraph
                :children
                :text)
  (:import-from :common-doc.util
                :make-text)
  (:export :*paragraph-separator-regex*
           :has-paragraph-separator
           :split-paragraph)
  (:documentation "Main package of split-paragraphs."))
(in-package :common-doc.split-paragraphs)

(defparameter *paragraph-separator-regex*
  (ppcre:create-scanner "\\n\\n")
  "A regular expression that matches double newlines.")

(defparameter +paragraph-marker+
  :paragraph-split
  "A marker that is inserted between strings after they are separated by
paragraphs.")

(defun has-paragraph-separator (string)
  "Does string contain a paragraph separator?"
  (if (ppcre:scan *paragraph-separator-regex* string) t))

(defun split-paragraph (string)
  "Split a string by the separator into a list of strings, or return the intact
string if it has none."
  (if (has-paragraph-separator string)
      (ppcre:split "\\n\\n" string)
      string))

(defun excise-paragraph-separators (list)
  "Take a list of text nodes or other elements. Separate strings by paragraphs,
leaving a paragraph marker between each string."
  (let ((output (list)))
    (loop for elem in list do
      (if (typep elem 'text-node)
          ;; If it's a text node, split it by its separator
          (let ((split (split-paragraph (text elem))))
            (if (stringp split)
                ;; Just a regular text node with no separators, add it to the
                ;; output
                (push elem output)
                ;; A list of text nodes, add each to the output, followed by a
                ;; paragraph separator marker
                (loop for text in split do
                  (push (make-text text) output)
                  (push +paragraph-marker+ output))))
          ;; If it's another node, add it to the output unconditionally
          (push elem output)))
    output))

(defun has-paragraph-markers (list)
  "Return whether a list has paragraph markers."
  (if (member +paragraph-marker+ list) t))

(defun group-into-paragraph-nodes (list)
  "Take a list of nodes separated by paragraph markers and merge them into
paragraph nodes."
  (if (has-paragraph-markers list)
      (let ((output (list))
            (current-paragraph-contents (list)))
        (loop for elem in list do
          (if (eql elem +paragraph-marker+)
              ;; End of the paragraph
              (progn
                (push (make-instance 'paragraph
                                     :children current-paragraph-contents)
                      output)
                (setf current-paragraph-contents nil))
              ;; Another node, so just push it in the paragraph
              (push elem current-paragraph-contents)))
        output)
      list))
