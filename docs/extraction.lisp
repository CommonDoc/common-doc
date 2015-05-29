(defpackage extraction-example
  (:use :cl :common-doc)
  (:import-from :common-doc.ops
                :collect-figures))
(in-package :extraction-example)

(defvar *document*
  (make-document "test"
                 :children
                 (list
                  (make-section
                   (list (make-text "Section 1"))
                   :children
                   (list
                    (make-figure
                     (make-image "fig1.jpg")
                     (list
                      (make-text "Fig 1")))))
                  (make-section
                   (list (make-text "Section 2"))
                   :children
                   (list
                    (make-figure
                     (make-image "fig2.jpg")
                     (list
                      (make-text "Fig 2"))))))))

(collect-figures *document*) ;; => (#<FIGURE {1009913D83}> #<FIGURE {1009A98923}>)
