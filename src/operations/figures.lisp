(in-package :common-doc.ops)

(defun collect-figures (doc-or-node)
  "Return a list of figures in the document."
  (let ((figures (list)))
    (traverse-document doc-or-node
                       #'(lambda (node)
                           (when (typep node 'figure)
                             (push node figures))))
    (reverse figures)))
