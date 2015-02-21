(in-package :common-doc.ops)

(defun fill-unique-refs (doc-or-node)
  "Recur through a document, giving unique reference IDs to each section."
  (let ((table (make-hash-table))
        (current-pos 0))
    ;; 'table' is a hash table that maps the position of a section (0 for first,
    ;; 1 for second, etc.) to a unique section ID
    (labels ((slug-in-table-p (slug)
               ;; Determine if 'slug' is in the table.
               (member slug
                       (alexandria:hash-table-values table)
                       :test #'equal))
             (add-section-reference (section)
               ;; Extract a unique slug from a section's title, and add it to
               ;; the table to the section.
               (let* ((section-text (common-doc.ops:collect-all-text (title section)))
                      (section-slug (common-doc.util:string-to-slug section-text))
                      (final-slug (if (slug-in-table-p section-slug)
                                      (concatenate 'string
                                                   (write-to-string current-pos)
                                                   "-"
                                                   section-slug)
                                      section-slug)))
                 ;; Add it to the table
                 (setf (gethash current-pos table) final-slug)
                 ;; Set the section's reference to the slug
                 (setf (reference section) final-slug)
                 (incf current-pos))))
      (with-document-traversal (doc-or-node node)
        (when (typep node 'section)
          (add-section-reference node)))
      doc-or-node)))
