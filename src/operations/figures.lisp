(in-package :common-doc.ops)

(defun collect-figures (doc-or-node)
  "Return a list of figures in the document."
  (let ((figures (list)))
    (with-document-traversal (doc-or-node node)
      (when (typep node 'figure)
        (push node figures)))
    (reverse figures)))

(defun collect-images (doc-or-node)
  "Return a list of images in the document."
  (let ((images (list)))
    (with-document-traversal (doc-or-node node)
      (when (typep node 'image)
        (push node images))
      (when (typep node 'figure)
        (push (image node) images)))
    (reverse images)))
