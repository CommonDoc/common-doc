(in-package :common-doc)

;;; Utilities

(defun construct (class children metadata)
  "Instantiate a class with children and metadata."
  (make-instance class :children children :metadata metadata))

;;; Interface

(defun make-meta (pairs)
  "Create a metadata table from a list of pairs. If the list is empty, return an
empty metadata table."
  (let ((table (make-hash-table :test #'equal)))
    (loop for pair in pairs do
      (setf (gethash (first pair) table) (rest pair)))
    table))

(defun make-content (children &key metadata)
  "Create a content node from its children."
  (construct 'content-node children metadata))

(defun make-text (string &key metadata)
  "Create a text node from the contents of a string."
  (make-instance 'text-node
                 :text string
                 :metadata metadata))

(defun make-paragraph (children &key metadata)
  "Create a paragraph node from its children."
  (construct 'paragraph children metadata))

(defun make-bold (children &key metadata)
  "Create a bold node from its children."
  (construct 'bold children metadata))

(defun make-italic (children &key metadata)
  "Create an italicized node from its children."
  (construct 'italic children metadata))

(defun make-underline (children &key metadata)
  "Create an underlined node from its children."
  (construct 'underline children metadata))

(defun make-strikethrough (children &key metadata)
  "Create an striked out node from its children."
  (construct 'strikethrough children metadata))

(defun make-code (children &key metadata)
  "Create an inline code node from its children."
  (construct 'code children metadata))

(defun make-superscript (children &key metadata)
  "Create a superscripted node from its children."
  (construct 'superscript children metadata))

(defun make-subscript (children &key metadata)
  "Create a subscripted node from its children."
  (construct 'subscript children metadata))

(defun make-code-block (language children &key metadata)
  "Create a code block node from its children and language."
  (make-instance 'code-block
                 :language language
                 :children children
                 :metadata metadata))

(defun make-inline-quote (children &key metadata)
  "Create an inline quote node from its children."
  (construct 'inline-quote children metadata))

(defun make-block-quote (children &key metadata)
  "Create a block quote node from its children."
  (construct 'block-quote children metadata))

(defun make-document-link (document section children &key metadata)
  "Create a document link from document and section references and its children."
  (make-instance 'document-link
                 :document-reference document
                 :section-reference section
                 :children children
                 :metadata metadata))

(defun make-web-link (uri children &key metadata)
  "Create a web link."
  (make-instance 'web-link
                 :uri (quri:uri uri)
                 :children children
                 :metadata metadata))

(defun make-list-item (children &key metadata)
  "Create a list item."
  (construct 'list-item children metadata))

(defun make-definition (term definition &key metadata)
  "Create a definition list item."
  (make-instance 'definition
                 :term term
                 :definition definition
                 :metadata metadata))

(defun make-unordered-list (children &key metadata)
  "Create an unordered list."
  (construct 'unordered-list children metadata))

(defun make-ordered-list (children &key metadata)
  "Create an ordered list."
  (construct 'ordered-list children metadata))

(defun make-definition-list (children &key metadata)
  "Create a definition list."
  (construct 'definition-list children metadata))

(defun make-image (source &key description metadata)
  "Create an image."
  (make-instance 'image
                 :source source
                 :description description
                 :metadata metadata))

(defun make-figure (image description &key metadata)
  "Create a figure."
  (make-instance 'figure
                 :image image
                 :description description
                 :metadata metadata))

(defun make-table (rows &key metadata)
  "Create a table from a list of rows."
  (make-instance 'table
                 :rows rows
                 :metadata metadata))

(defun make-row (cells &key metadata)
  "Create a row from a list of cells."
  (make-instance 'row
                 :cells cells
                 :metadata metadata))

(defun make-cell (children &key metadata)
  "Create a cell from its children."
  (construct 'cell children metadata))

(defun make-section (title &key children reference metadata)
  "Create a section from its title and children."
  (make-instance 'section
                 :title title
                 :reference reference
                 :children children
                 :metadata metadata))

(defun make-document (title &key children keywords &allow-other-keys)
  "Create a document."
  (make-instance 'document
                 :title title
                 :children children
                 :keywords keywords))
