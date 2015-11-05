;;;; Metadata interface
(in-package :common-doc)

(defun make-meta (pairs)
  "Create a metadata table from a list of pairs. If the list is empty, return an
empty metadata table."
  (let ((table (make-hash-table :test #'equal)))
    (loop for pair in pairs do
      (setf (gethash (first pair) table) (rest pair)))
    table))

(defun get-meta (node key)
  "Find the value corresponding to @cl:param(key) in the node's metadata. If not
found, return @c(NIL)."
  (when (metadata node)
    (gethash key (metadata node))))

(defun (setf get-meta) (value node key)
  (if (metadata node)
      (setf (gethash key (metadata node)) value)
      (progn
        (setf (metadata node)
              (make-hash-table :test #'equal))
        (setf (get-meta node key) value))))

(defmacro do-meta ((key value node) &body body)
  "Iterate over the keys and values of a node's metadata."
  (let ((meta (gensym)))
    `(let ((,meta (metadata ,node)))
       (loop for ,key being the hash-keys of ,meta
             for ,value being the hash-values of ,meta
             do (progn ,@body)))))
