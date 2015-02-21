(in-package :common-doc.ops)

(defgeneric toc-traverse (node)
  (:documentation "A function that traverses a document tree looking for section
  nodes.")
  (:method ((doc document))
    (toc-traverse (children doc)))
  (:method ((node content-node))
    (toc-traverse (children node)))
  (:method ((sec section))
    (list :sec sec
          :children (toc-traverse (children sec))))
  (:method ((list list))
    (remove-if #'null
               (loop for elem in list collecting
                 (toc-traverse elem))))
  (:method ((obj t))
    nil))

(defun un-nest (node)
  (cond
    ((null node)
     node)
    ((listp node)
     (if (eql (length node) 1)
         (un-nest (first node))
         (loop for child in node collecting
           (un-nest child))))
    (t
     node)))

(defun extract (node)
  (if (listp node)
      (if (eq (first node) :sec)
          (let ((sec (getf node :sec)))
            (make-instance 'document-link
                           :section-reference (reference sec)
                           :children (append
                                      (list
                                       (make-instance 'content-node
                                                      :children (list (title sec))))
                                      (let ((children (extract (getf node :children))))
                                        (if (listp children)
                                            children
                                            (list children))))))
          (loop for child in node collecting
            (extract child)))
      node))

(defun table-of-contents (doc-or-node)
  "Extract a tree of document links representing the table of contents of a
document."
  (let ((toc (extract (un-nest (toc-traverse doc-or-node)))))
    (make-instance 'content-node
                   :metadata (common-doc.util:make-meta (list (cons "class" "toc")))
                   :children toc)))
