(in-package :common-doc.ops)

(defgeneric toc-traverse (node)
  (:documentation "A function that traverses a document tree looking for section
  nodes.")
  (:method ((doc document))
    (toc-traverse (children doc)))
  (:method ((node content-node))
    (toc-traverse (children node)))
  (:method ((sec section))
    (list :title (title sec)
          :reference (reference sec)
          :children (toc-traverse (children sec))))
  (:method ((list list))
    (remove-if #'null
               (loop for elem in list collecting
                 (toc-traverse elem))))
  (:method ((obj t))
    nil))

(defun un-nest (node)
  (if (atom node)
      node
      (if (eql (length node) 1)
          (un-nest (first node))
          (if (eq (first node) :title)
              (list :title (getf node :title)
                    :reference (getf node :reference)
                    :children (let ((un-nested (un-nest (getf node :children))))
                                (if un-nested
                                    (list un-nested)
                                    nil)))
              (loop for elem in node collecting
                (un-nest elem))))))

(defun table-of-contents (doc-or-node)
  "Extract a nested plist representing the table of contents of a document."
  (un-nest (toc-traverse doc-or-node)))
