(in-package :common-doc)

(defmacro doc (class args &rest children)
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
