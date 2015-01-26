(in-package :common-doc-test)

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
