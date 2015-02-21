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
               ;; Extract a unique ref from a section's title, and add it to
               ;; the table to the section.
               (let* ((section-text (common-doc.ops:collect-all-text (title section)))
                      (section-ref (common-doc.util:string-to-ref section-text))
                      (final-ref (if (ref-in-table-p section-ref)
                                     (concatenate 'string
                                                  (write-to-string current-pos)
                                                  "-"
                                                  section-ref)
                                     section-ref)))
                 ;; Add it to the table
                 (setf (gethash current-pos table) final-ref)
                 ;; Set the section's reference to the ref
                 (setf (reference section) final-ref)
                 (incf current-pos))))
      (with-document-traversal (doc-or-node node)
        (when (typep node 'section)
          (add-section-reference node)))
      doc-or-node)))
