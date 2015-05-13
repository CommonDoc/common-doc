(in-package :common-doc-test.contrib)

(test split-paragraphs
  (let ((node
          (make-content
           (list
            (make-text (format nil "Paragraph 1.~%~%"))
            (make-text "Paragraph with ")
            (make-bold
             (list (make-text "bold text")))
            (make-text (format nil ".~%~%"))
            (make-text "Paragraph 3.")))))
    (finishes
      (common-doc.split-paragraphs:split-paragraphs node))))
