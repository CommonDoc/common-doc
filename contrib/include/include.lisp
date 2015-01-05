(in-package :cl-user)
(defpackage common-doc.include
  (:use :cl)
  (:import-from :common-doc.macro
                :<macro-node>
                :expand-macro)
  (:import-from :common-doc
                :define-node)
  (:import-from :common-doc.util
                :make-text)
  (:documentation "Includex package."))
(in-package :common-doc.include)

;;; Utilities

(defun parse-range (range-string)
  "Return nil or a `(start-line, end-line)` pair."
  (if range-string
      ;; Actual parsing
      (cons 1 2)
      ;; If the input is nil, just return nil
      nil))

;;; Classes

(define-node <include> (<macro-node>)
  ((path :reader include-path
         :initarg :path
         :type string
         :attribute-name "path"
         :documentation "Path to the local file to include.")
   (range :reader include-range
          :initarg :range
          :initform nil
          :type string
          :attribute-name "range"
          :documentation "The range of lines to include."))
  (:tag-name "include")
  (:documentation "Include an external file."))

;;; Macroexpansions

(defmethod expand-macro ((include <include>))
  "Expand the include file into a text node with its contents."
  (let* ((path (common-doc.file:absolute-path (include-path include)))
         (range (parse-range (include-range include)))
         (full-text (uiop:read-file-string path)))
    (if (null range)
        ;; No range, so we just return everything as a chunk of text
        (make-text full-text)
        ;; Otherwise, select the range we want
        (let* ((lines (split-sequence:split-sequence #\Newline full-text))
               (lines-in-range (subseq lines
                                       (1- (first range))
                                       (rest range))))
          (make-text (reduce
                      #'(lambda (a b) (concatenate 'string a b))
                      lines-in-range))))))
