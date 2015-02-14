(in-package :common-doc-test)

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
          (t
           t))))
    (is
     (equal document-depth 1))))

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
