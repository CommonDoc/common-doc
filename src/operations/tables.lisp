(in-package :common-doc.ops)

(defmethod collect-tables ((doc document))
  "Return a list of tables in the document."
  (let ((tables (list)))
    (traverse-document doc
                       #'(lambda (node)
                           (when (typep node 'table)
                             (push node tables))))
    (reverse tables)))
