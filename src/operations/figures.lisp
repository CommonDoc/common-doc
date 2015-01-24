(in-package :common-doc.ops)

(defmethod collect-figures ((doc document))
  "Return a list of figures in the document."
  (let ((figures (list)))
    (traverse-document doc
                       #'(lambda (node)
                           (when (typep node 'figure)
                             (push node figures))))
    (reverse figures)))
