(in-package :cl-user)
(defpackage common-doc-test
  (:use :cl :fiveam)
  (:import-from :common-doc
                :doc
                :<document>
                :<paragraph>
                :<text-node>))
(in-package :common-doc-test)

(def-suite tests
  :description "common-doc tests.")
(in-suite tests)

(defun extract-doc-text (doc)
  (with-output-to-string (stream)
    (common-doc:traverse-document
     doc
     #'(lambda (node)
         (when (typep node '<text-node>)
           (write-string (common-doc:text node) stream))))))

(test simple-doc
  (let ((document
          (doc
           <document>
           (:title "My Document"
            :creator "me"
            :keywords (list "test" "test1"))
           (<paragraph>
            ()
            (<text-node>
             (:text "test"))))))
    (is
     (equal (common-doc:keywords document) (list "test" "test1")))
    (is (equal (extract-doc-text document)
               "test"))))

(run! 'tests)
