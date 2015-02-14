(in-package :common-doc.ops)

(defun collect-external-links (doc-or-node)
  "Return a list of external links in the document."
  (let ((links (list)))
    (traverse-document doc-or-node
                       #'(lambda (node)
                           (when (typep node 'web-link)
                             (push node links))))
    (reverse links)))
