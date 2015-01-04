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

(defmethod node-equal ((node-a <document-node>)
                       (node-b <document-node>))
  "Recursively check whether two nodes are equal."
  (let ((metadata-a (metadata node-a))
        (metadata-b (metadata node-b)))
    (and
     ;; First, the obvious: We verify they are both the same kind of node
     (equal (class-of node-a)
            (class-of node-b))
     ;; If the nodes are the same, we verify the metadata match. Either both
     ;; metadata are null, or they are equal hash tables.
     (or (and (null metadata-a)
              (null metadata-b))
         (hash-table-equal metadata-a metadata-b))
     ;; Now we verify the children, if any
     (if (subtypep node-a '<content-node>)
         ;; If they have children, recursively check them
         (let ((children-a (children node-a))
               (children-b (children node-b)))
           (every #'identity
                  (loop for child-a in children-a
                        for child-b in children-b
                        collecting
                        (node-equal child-a child-b))))
         ;; Otherwise, just return t
         t))))
