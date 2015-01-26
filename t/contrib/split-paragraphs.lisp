(in-package :common-doc-test)

(test split-paragraphs
  (let ((node
          (doc
           content-node
           ()
           (text-node
            (:text (format nil "Paragraph 1.~%~%")))
           (text-node
            (:text (format nil "Paragraph with ")))
           (bold
            ()
            (text-node
             (:text "bold text")))
           (text-node
            (:text (format nil ".~%~%")))
           (text-node
            (:text (format nil "Paragraph 3."))))))))
