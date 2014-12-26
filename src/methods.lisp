(in-package :common-doc)

(defgeneric traverse-document (node function)
  (:documentation "Apply a side effectful function recursively to every element
  in the document. Depth-first.")

  (:method ((doc <document>) function)
    (funcall function doc)
    (loop for child in (content doc) do
      (traverse-document child function)))

  (:method ((cnode <content-node>) function)
    (funcall function cnode)
    (loop for child in (children cnode) do
      (traverse-document child function)))

  (:method ((dnode <document-node>) function)
    (funcall function dnode)))

(defmethod collect-figures ((doc <document>))
  "Return a list of figures in the document."
  (let ((figures (list)))
    (traverse-document doc
                       #'(lambda (node)
                           (when (typep node '<figure>)
                             (push node figures))))
    figures))

(defmethod collect-tables ((doc <document>))
  "Return a list of tables in the document."
  (let ((tables (list)))
    (traverse-document doc
                       #'(lambda (node)
                           (when (typep node '<table>)
                             (push node tables))))
    tables))
