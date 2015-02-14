(in-package :common-doc.ops)

(defgeneric node-text (node)
  (:documentation "Extract a string with all the text inside a node."))

(defmethod node-text ((node content-node))
  "Extract text from a content node."
  (node-text (children node)))

(defmethod node-text ((list list))
  "Extract text from a list of nodes."
  (reduce #'(lambda (a b)
              (concatenate 'string a b))
          (loop for elem in list collecting
            (node-text elem))))

(defmethod node-text ((text text-node))
  "Extract text from a text node."
  (text text))

(defmethod node-text ((def definition))
  "Extract text from a definition."
  (concatenate 'string
               (node-text (term def))
               " "
               (node-text (definition def))))

(defmethod node-text ((image image))
  "Extract the description from an image."
  (description image))

(defmethod node-text ((fig figure))
  "Extract the description from a figure."
  (concatenate 'string
               (node-text (image fig))
               " "
               (node-text (description fig))))

(defmethod node-text ((row row))
  "Extract text from a row of cells."
  (node-text (cells row)))

(defmethod node-text ((table table))
  "Extract text from a table."
  (node-text (rows table)))

(defmethod node-text ((section section))
  "Extract text from a section."
  (concatenate 'string
               (node-text (title section))
               " "
               (node-text (children section))))

(defmethod node-text ((doc document))
  "Extract text from a document."
  (node-text (children doc)))

(defun collect-all-text (doc-or-node)
  "Return all the text from a node or document."
  (node-text doc-or-node))
