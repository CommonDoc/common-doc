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

(test extract-figures
  (let ((document)
        (figs))
    (finishes
      (setf document
            (doc
             document
             ()
             (section
              (:title (make-text "Section 1"))
              (figure
               (:image (doc image (:source "fig1.jpg"))
                :description
                (list
                 (doc
                  text-node
                  (:text "Fig 1"))))))
             (section
              (:title (make-text "Section 2"))
              (figure
               (:image (doc image (:source "fig2.jpg"))
                :description
                (list
                 (doc
                  text-node
                  (:text "Fig 2")))))))))
    (finishes
      (setf figs (collect-figures document)))
    (let* ((first-fig (first figs))
           (second-fig (second figs))
           (first-img (image first-fig))
           (second-img (image second-fig)))
      (is
       (equal (source first-img) "fig1.jpg"))
      (is
       (equal (source second-img) "fig2.jpg")))))

(test node-equality
  (is
   (node-equal (make-text "test")
               (make-text "test")))
  (let* ((image
           (doc image (:source "fig1.jpg")))
         (paragraph
           (doc
            paragraph
            ()
            (text-node
             (:text "test"))))
         (section
           (doc
            section
            (:title (make-text "Section 1"))
            (figure
             (:image image
              :description (list paragraph))))))
    (macrolet ((tests (&rest nodes)
                 `(progn
                    ,@(loop for node in nodes collecting
                            `(is (node-equal ,node ,node)))
                    ,@(loop for node in nodes collecting
                            `(progn
                               ,@(loop for other-node in (set-difference nodes (list node))
                                       collecting
                                       `(is (not (node-equal ,node ,other-node)))))))))
      (tests image paragraph section))))

(run! 'tests)
