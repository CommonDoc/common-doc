(in-package :common-doc.ops)

(defmethod collect-external-links ((doc document))
  "Return a list of external links in the document."
  (let ((links (list)))
    (traverse-document doc
                       #'(lambda (node)
                           (when (typep node 'web-link)
                             (push node links))))
    (reverse links)))
