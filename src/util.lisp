(in-package :common-doc.util)

(defmacro doc (class args &rest children)
  "Easily create a document or node.

  `(doc subscript ())`

  is equivalent to:

  `(make-instance 'subscript)`

  `(doc document (:title \"My Document\") (text-node (:text \"...\")))`

  is equivalent to:

  `(make-instance 'document :title \"My Document\" :children (list (make-instance 'text-node :text \"...\")))`"
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

(defun make-meta (pairs)
  "Create a metadata hash table from a list of cons cells."
  (let ((table (make-hash-table :test #'equal)))
    (loop for pair in pairs do
      (setf (gethash (first pair) table) (rest pair)))
    table))

(defun make-text (string &optional metadata)
  "Create a text node from the contents of a string."
  (make-instance 'text-node :text string :metadata metadata))

(defun string-to-slug (string)
  "Take a string, usually the name of a section, and create something that is
more similar to an identifier, i.e. no spaces, same case, etc."
  (let* ((no-space (substitute #\- #\Space string))
         (no-slashes (substitute #\- #\/ no-space)))
    (string-downcase no-slashes)))
