(in-package :common-doc.ops)

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

(defun node-list-equal (list-a list-b)
  (every #'identity
         (loop for node-a in list-a
               for node-b in list-b
               collecting
               (node-equal node-a node-b))))

(defmethod node-children-equal ((node-a document-node)
                                (node-b document-node))
  "Recursively check for equality in the children of a node."
  (if (subtypep (class-of node-a) 'content-node)
      ;; If they have children, recursively check them
      (node-list-equal (children node-a) (children node-b))
      t))

(defmethod node-metadata-equal ((node-a document-node)
                                (node-b document-node))
  "Check whether two nodes have the same metadata."
  (let ((metadata-a (metadata node-a))
        (metadata-b (metadata node-b)))
     ;; If the nodes are the same, we verify the metadata match. Either both
     ;; metadata are null, or they are equal hash tables.
    (if (and (null metadata-a)
             (null metadata-b))
        t
        (if (or (null metadata-a)
                (null metadata-b))
            nil
            (hash-table-equal metadata-a metadata-b)))))

(defgeneric node-specific-equal (node-a node-b)
  (:documentation "Use this method to make node equality more specific."))

(defmethod node-specific-equal ((node-a document-node)
                                (node-b document-node))
  "By default, return true."
  t)

(defmethod node-equal ((node-a document-node)
                       (node-b document-node))

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

(defmethod node-specific-equal ((text-a text-node)
                                (text-b text-node))
  (equal (text text-a) (text text-b)))

(defmethod node-specific-equal ((code-a code-block)
                                (code-b code-block))
  (equal (language code-a) (language code-b)))

(defmethod node-specific-equal ((link-a document-link)
                                (link-b document-link))
  (and
   (equal (document-reference link-a) (document-reference link-b))
   (equal (node-reference link-a) (node-reference link-b))))

(defmethod node-specific-equal ((link-a web-link)
                                (link-b web-link))
  (quri:uri= (uri link-a) (uri link-b)))

(defmethod node-specific-equal ((definition-a definition)
                                (definition-b definition))
  (and (node-list-equal (term definition-a)
                        (term definition-b))
       (node-list-equal (definition definition-a)
                        (definition definition-b))))

(defmethod node-specific-equal ((image-a image)
                                (image-b image))
  (and (equal (source image-a) (source image-b))
       (equal (description image-a) (description image-b))))

(defmethod node-specific-equal ((figure-a figure)
                                (figure-b figure))
  (and (node-equal (image figure-a)
                   (image figure-b))
       (node-list-equal (description figure-a)
                        (description figure-b))))

(defmethod node-specific-equal ((section-a section)
                                (section-b section))
  (and (equal (title section-a)
              (title section-b))
       (equal (reference section-a)
              (reference section-b))))
