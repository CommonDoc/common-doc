(in-package :cl-user)
(defpackage common-doc-test.ops
  (:use :cl :fiveam :common-doc)
  (:import-from :common-doc.util
                :doc
                :make-text)
  (:import-from :common-doc.ops
                :with-document-traversal
                :collect-figures
                :node-equal)
  (:export :tests))
(in-package :common-doc-test.ops)

(def-suite tests
  :description "common-doc operations tests.")
(in-suite tests)

(test traverse-depth
  (let ((document
          (doc
           document
           ()
           (bold
            ()
            (italic
             ()
             (underline
              ())))))
        (document-depth)
        (bold-depth)
        (italic-depth)
        (underline-depth))
    (finishes
      (with-document-traversal (document node depth)
        (typecase node
          (document
           (setf document-depth depth))
          (bold
           (setf bold-depth depth))
          (italic
           (setf italic-depth depth))
          (underline
           (setf underline-depth depth)))))
    (is
     (equal document-depth 0))
    (is
     (equal bold-depth 1))
    (is
     (equal italic-depth 2))
    (is
     (equal underline-depth 3))))

(test extract-figures
  (let ((document)
        (figs))
    (finishes
      (setf document
            (doc
             document
             ()
             (section
              (:title (list (make-text "Section 1")))
              (figure
               (:image (doc image (:source "fig1.jpg"))
                :description
                (list
                 (doc
                  text-node
                  (:text "Fig 1"))))))
             (section
              (:title (list (make-text "Section 2")))
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

(test unique-refs
  (let ((doc (doc
              document
              ()
              (section
               (:title (list (make-text "Section 1")))
               (content-node
                ()
                (content-node
                 ()
                 (section
                  (:title (list (make-text "Section 1.1"))
                   :reference "sec11")))))
              (section
               (:title (list (make-text "Section 2")))))))
    (finishes
      (common-doc.ops:fill-unique-refs doc))
    (is
     (equal (reference (first (children doc)))
            "section-1"))
    (is
     (equal (reference (first (children
                               (first (children
                                       (first (children
                                               (first (children doc)))))))))
            "sec11"))
    (is
     (equal (reference (second (children doc)))
            "section-2"))))

(test toc
  (let* ((doc (doc
               document
               ()
               (section
                (:title (make-text "Section 1")
                 :reference "sec1")
                (content-node
                 ()
                 (content-node
                  ()
                  (section
                   (:title (make-text "Section 1.1")
                    :reference "sec11")))))
               (section
                (:title (make-text "Section 2")
                 :reference "sec2"))))
         (toc (common-doc.ops:table-of-contents doc)))
    (is-true (typep toc 'ordered-list))
    (is
     (equal (length (children toc))
            2))
    (let ((list-item (first (children toc))))
      (is-true (typep list-item 'list-item)))
    (let ((list-item (second (children toc))))
      (is-true (typep list-item 'list-item))
      (let ((link (first (children list-item))))
        (is
         (equal (common-doc.ops:collect-all-text link)
                "Section 2"))))))
