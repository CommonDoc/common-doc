(in-package :common-doc.ops)

(defgeneric traverse-document (node function)
  (:documentation "Apply a side effectful function recursively to every element
  in the document. Depth-first.")

  (:method ((doc document) function)
    (funcall function doc)
    (loop for child in (children doc) do
      (traverse-document child function)))

  (:method ((cnode content-node) function)
    (funcall function cnode)
    (loop for child in (children cnode) do
      (traverse-document child function)))

  (:method ((dnode document-node) function)
    (funcall function dnode)))
