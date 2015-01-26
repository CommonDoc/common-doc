(in-package :cl-user)
(defpackage common-doc-test
  (:use :cl :fiveam :common-doc)
  (:import-from :common-doc.util
                :doc
                :make-text)
  (:import-from :common-doc.ops
                :traverse-document
                :collect-figures
                :node-equal))
(in-package :common-doc-test)

(def-suite tests
  :description "common-doc tests.")
(in-suite tests)

(defun extract-doc-text (doc)
  (with-output-to-string (stream)
    (traverse-document
     doc
     #'(lambda (node)
         (when (typep node 'text-node)
           (write-string (text node) stream))))))

(test simple-doc
  (let ((document
          (doc
           document
           (:title "My Document"
            :creator "me"
            :keywords (list "test" "test1"))
           (paragraph
            ()
            (text-node
             (:text "test"))))))
    (is
     (equal (keywords document) (list "test" "test1")))
    (is (equal (extract-doc-text document)
               "test"))))
