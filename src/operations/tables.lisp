(in-package :common-doc.ops)

(defun collect-tables (doc-or-node)
  "Return a list of tables in the document."
  (let ((tables (list)))
    (with-document-traversal (doc-or-node node)
      (when (typep node 'table)
        (push node tables)))
    (reverse tables)))
