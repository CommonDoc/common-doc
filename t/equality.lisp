(in-package :common-doc-test.ops)

(test node-equality
  (is
   (node-equal (make-text "test")
               (make-text "test")))
  (let* ((image (make-image "fig1.jpg"))
         (paragraph (make-paragraph
                     (list (make-text "test"))))
         (section (make-section
                   (list (make-text "Section 1"))
                   :children
                   (list
                    (make-figure
                     image
                     (list paragraph))))))
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
