(defpackage split-paragraphs-example
  (:use :cl :common-doc)
  (:import-from :common-doc.split-paragraphs
                :split-paragraphs))
(in-package :split-paragraphs-example)

(defvar *document* (make-content
                    (list
                     (make-text
                      (format nil "Some text.~%~%Some "))
                     (make-bold
                      (list (make-text "bold text")))
                     (make-text (format nil ".~%~%Other text.")))))

(dump *document*)

;; content-node
;;   text-node
;;     "Some text.
;;
;; Some "
;;   bold
;;     text-node
;;       "bold text"
;;   text-node
;;     ".
;;
;; Other text."

(setf *document* (split-paragraphs *document*))

(dump *document*)

;; content-node
;;   paragraph
;;     text-node
;;       "Some text."
;;   paragraph
;;     text-node
;;       "Some "
;;     bold
;;       text-node
;;         "bold text"
;;     text-node
;;       "."
;;   paragraph
;;     text-node
;;       "Other text."
