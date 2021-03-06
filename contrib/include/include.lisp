(in-package :cl-user)
(defpackage common-doc.include
  (:use :cl)
  (:import-from :anaphora
                :aif
                :it)
  (:import-from :split-sequence
                :split-sequence)
  (:import-from :common-doc.macro
                :macro-node
                :expand-macro)
  (:import-from :common-doc
                :define-node)
  (:import-from :common-doc.util
                :make-text)
  (:export :include
           :include-path
           :include-start
           :include-end)
  (:documentation "Includex package."))
(in-package :common-doc.include)

;;; Classes

(define-node include (macro-node)
  ((path :reader include-path
         :initarg :path
         :type string
         :attribute-name "path"
         :documentation "Path to the local file to include.")
   (start :reader include-start
          :initarg :start
          :initform nil
          :type (or null string)
          :attribute-name "start"
          :documentation "The line where the inclusion will start.")
   (end :reader include-end
        :initarg :end
        :initform nil
        :type (or null string)
        :attribute-name "end"
        :documentation "The line where the inclusion will end."))
  (:tag-name "include")
  (:documentation "Include an external file."))

;;; Macroexpansions

(defmethod expand-macro ((include include))
  "Expand the include file into a text node with its contents."
  (let* ((path (common-doc.file:absolute-path (include-path include)))
         (start (aif (include-start include)
                     (parse-integer it :junk-allowed t)
                     nil))
         (end (aif (include-end include)
                   (parse-integer it :junk-allowed t)
                   nil))
         (full-text (uiop:read-file-string path)))
    (if (or start end)
        ;; We have at least some range information
        (let ((lines (split-sequence #\Newline full-text)))
          (flet ((make-text-from-lines (lines)
                   (make-text (reduce
                               #'(lambda (a b)
                                   (concatenate 'string a (string #\Newline) b))
                               lines))))
            (cond
              ((and start (not end))
               ;; Start at a line, and go to the end
               (make-text-from-lines (subseq lines (1- start))))
              ((and (not start) end)
               ;; Start at 0, go to the end
               (make-text-from-lines (subseq lines 0)))
              (t
               ;; Full range, select the text we want
               (make-text-from-lines (subseq lines (1- start) end))))))
        ;; We have no range information, return the full text
        (make-text full-text))))
