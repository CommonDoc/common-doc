(in-package :common-doc)

(defmacro doc (class args &rest children)
  "Easily create a document or node.

  `(doc <subscript> ())`

  is equivalent to:

  `(make-instance '<subscript>)`

  `(doc <document> (:title \"My Document\") (<text-node> (:text \"...\")))`

  is equivalent to:

  `(make-instance '<document> :title \"My Document\" :children (list (make-instance '<text-node> :text \"...\")))`"
  (labels ((recur (args)
             (destructuring-bind (class args &rest children) args
               (if children
                   `(make-instance ',class
                                   ,@args
                                   :children (list
                                              ,@(loop for child in children collecting
                                                  (recur child))))
                   `(make-instance ',class ,@args)))))
    (recur (cons class (cons args children)))))
