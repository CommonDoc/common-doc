(defpackage toc-example
  (:use :cl :common-doc)
  (:import-from :common-doc.ops
                :table-of-contents))
(in-package :toc-example)

(defvar *document*
  (make-document "test"
                 :children
                 (list
                  (make-section
                   (list (make-text "Section 1"))
                   :reference "sec1"
                   :children
                   (list
                    (make-content
                     (list
                      (make-content
                       (list
                        (make-section
                         (list (make-text "Section 1.1"))
                         :reference "sec11")))))))
                  (make-section
                   (list (make-text "Section 2"))
                   :reference "sec2"
                   :children
                   (list
                    (make-text "sec2 contents"))))))

(defvar *toc* (table-of-contents *document*))

(dump *toc*)
;; ordered-list [class=toc]
;;   list-item
;;     content-node
;;       document-link
;;         text-node
;;           "Section 1"
;;       ordered-list
;;         list-item
;;           content-node
;;             document-link
;;               text-node
;;                 "Section 1.1"
;;   list-item
;;     content-node
;;       document-link
;;         text-node
;;           "Section 2"
