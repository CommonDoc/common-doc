(in-package :common-doc.ops)

(defgeneric traverse-document (node function)
  (:documentation "Apply a side effectful function recursively to every element
  in the document. Depth-first.")

  (:method ((doc <document>) function)
    (funcall function doc)
    (loop for child in (children doc) do
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
    (reverse figures)))

(defmethod collect-tables ((doc <document>))
  "Return a list of tables in the document."
  (let ((tables (list)))
    (traverse-document doc
                       #'(lambda (node)
                           (when (typep node '<table>)
                             (push node tables))))
    (reverse tables)))

(defun hash-table-key-equal (table-a table-b)
  "Check whether two hash tables have the same keys."
  (equal (alexandria:hash-table-keys table-a)
         (alexandria:hash-table-keys table-b)))

(defun hash-table-equal (table-a table-b)
  "Check whether two hash tables are equal."
  (and (hash-table-key-equal table-a table-b)
       (every #'identity
              (loop for key in (alexandria:hash-table-keys table-a) collecting
                (equal (gethash key table-a)
                       (gethash key table-b))))))

(defgeneric node-equal (node-a node-b)
  (:documentation "Recursively check whether two nodes are equal."))

(defmethod node-children-equal ((node-a <document-node>)
                                (node-b <document-node>))
  "Recursively check for equality in the children of a node."
  (if (subtypep (class-of node-a) '<content-node>)
      ;; If they have children, recursively check them
      (let ((children-a (children node-a))
            (children-b (children node-b)))
        (every #'identity
               (loop for child-a in children-a
                     for child-b in children-b
                     collecting
                     (node-equal child-a child-b))))
      t))

(defmethod node-metadata-equal ((node-a <document-node>)
                                (node-b <document-node>))
  "Check whether two nodes have the same metadata."
  (let ((metadata-a (metadata node-a))
        (metadata-b (metadata node-b)))
     ;; If the nodes are the same, we verify the metadata match. Either both
     ;; metadata are null, or they are equal hash tables.
     (or (and (null metadata-a)
              (null metadata-b))
         (hash-table-equal metadata-a metadata-b))))

(defgeneric node-specific-equal (node-a node-b)
  (:documentation "Use this method to make node equality more specific."))

(defmethod node-specific-equal ((node-a <document-node>)
                                (node-b <document-node>))
  "By default, return true."
  t)

(defmethod node-equal ((node-a <document-node>)
                       (node-b <document-node>))

  (and
   ;; First, the obvious: We verify they are both the same kind of node
   (equal (class-of node-a)
          (class-of node-b))
   ;; Now, we use other methods
   (node-metadata-equal node-a node-b)
   (node-children-equal node-a node-b)
   ;; And, finally
   (node-specific-equal node-a node-b)))

;; Specific equality to different methods

(defmethod node-specific-equal ((text-a <text-node>)
                                (text-b <text-node>))
  (equal (text text-a) (text text-b)))

(defmethod node-specific-equal ((code-a <code-block>)
                                (code-b <code-block>))
  (equal (language code-a) (language code-b)))

(defmethod node-specific-equal ((link-a <document-link>)
                                (link-b <document-link>))
  (and
   (equal (document-reference link-a) (document-reference link-b))
   (equal (section-reference link-a) (section-reference link-b))))

(defmethod node-specific-equal ((link-a <web-link>)
                                (link-b <web-link>))
  (quri:uri= (uri link-a) (uri link-b)))

(defmethod node-specific-equal ((definition-a <definition>)
                                (definition-b <definition>))
  (and (node-equal (term definition-a)
                   (term definition-b))
       (node-equal (definition definition-a)
                   (definition definition-b))))

(defmethod node-specific-equal ((image-a <image>)
                                (image-b <image>))
  (and (equal (source image-a) (source image-b))
       (equal (description image-a) (description image-b))))

(defmethod node-specific-equal ((figure-a <figure>)
                                (figure-b <figure>))
  (and (node-equal (image figure-a)
                   (image figure-b))
       (node-equal (description figure-a)
                   (description figure-b))))

(defmethod node-specific-equal ((section-a <section>)
                                (section-b <section>))
  (and (equal (title section-a)
                   (title section-b))
       (equal (reference section-a)
              (reference section-b))))
