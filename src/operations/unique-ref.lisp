(in-package :common-doc.ops)

(defun fill-unique-refs (doc-or-node)
  "Recur through a document, giving unique reference IDs to each section."
  (let ((taken (make-hash-table :test #'equal)))
    ;; `taken` is a hash table of node references to booleans
    (with-document-traversal (doc-or-node node)
      ;; Go through every node, noting the reference ID's that are taken
      (when (reference node)
        (setf (gethash (reference node) taken) t)))
    (let ((table (make-hash-table :test #'eql))
          (current-pos 0))
      ;; `table` is a hash table that maps the position of a section (0 for
      ;; first, 1 for second, etc.) to a unique section ID.
      (labels ((ref-in-table-p (ref)
                 ;; Determine if `ref` is in the table
                 (member ref
                         (alexandria:hash-table-values table)
                         :test #'equal))
               (takenp (ref)
                 ;; Determine if the reference is taken
                 (or (ref-in-table-p ref)
                     (gethash ref taken)))
               (generate-unique-ref (section)
                 ;; Generate a unique ref for a section
                 (let* ((title (common-doc.ops:collect-all-text (title section)))
                        (slug (common-doc.util:string-to-slug title)))
                   (loop while (takenp slug) do
                     (setf slug
                           (concatenate 'string
                                        (write-to-string current-pos)
                                        "-"
                                        slug))
                     (incf current-pos))
                   slug))
               (add-section-reference (section)
                 (let ((ref (generate-unique-ref section)))
                   ;; Add it to the table
                   (setf (gethash current-pos table) ref)
                   ;; Set the section's reference to the ref
                   (setf (reference section) ref))))
        (with-document-traversal (doc-or-node node)
          (when (and (typep node 'section)
                     (null (reference node)))
            (add-section-reference node)))
        doc-or-node))))
