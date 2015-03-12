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
  "Remove unnecessary nesting, ie: (((A))) => (A)."
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

(defun filter-depth (node max-depth)
  "Remove all nodes deeper than max-depth."
  (labels ((traverse (node depth)
             (if (listp node)
                 (if (eq (first node) :sec)
                     (if (>= depth max-depth)
                         (append
                          (alexandria:remove-from-plist node :children)
                          (list :children nil))
                         node)
                     (loop for child in node collecting
                       (traverse child (1+ depth))))
                 node)))
    (traverse node 1)))

(defun extract (node)
  (if (listp node)
      (if (eq (first node) :sec)
          (let ((sec (getf node :sec)))
            (make-instance 'content-node
                           :children
                           (append
                            (list (make-instance 'document-link
                                                 :section-reference (reference sec)
                                                 :children (title sec)))
                            (let ((children (extract (getf node :children))))
                              (if children
                                  (let ((children (if (listp children)
                                                      children
                                                      (list children))))
                                    (list
                                     (make-instance 'ordered-list
                                                    :children
                                                    (loop for child in children collecting
                                                       (make-instance 'list-item
                                                                      :children (list child)))))))))))
          (loop for child in node collecting
            (extract child)))
      node))

(defun table-of-contents (doc-or-node &key max-depth)
  "Extract a tree of document links representing the table of contents of a
document. All the sections in the document must have references, so you should
call fill-unique-refs first."
  (let* ((list (un-nest (toc-traverse doc-or-node)))
         (toc (extract (if max-depth
                           (filter-depth list max-depth)
                           list))))
    (make-instance 'ordered-list
                   :metadata (common-doc.util:make-meta (list (cons "class" "toc")))
                   :children (loop for child in (if (listp toc)
                                                    toc
                                                    (children toc))
                                   collecting
                               (make-instance 'list-item :children (list child))))))
