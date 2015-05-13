(in-package :cl-user)
(defpackage common-doc
  (:use :cl :trivial-types :anaphora)
  ;; Classes
  (:export :document-node
           :content-node
           :text-node
           :paragraph
           :markup
           :bold
           :italic
           :underline
           :strikethrough
           :code
           :superscript
           :subscript
           :code-block
           :base-quote
           :inline-quote
           :block-quote
           :link
           :document-link
           :web-link
           :base-list
           :list-item
           :definition
           :unordered-list
           :ordered-list
           :definition-list
           :image
           :figure
           :table
           :row
           :cell
           :section
           :document)
  ;; Accessors
  (:export :metadata
           :children
           :text
           :language
           :section-reference
           :document-reference
           :uri
           :term
           :definition
           :source
           :description
           :image
           :rows
           :header
           :footer
           :cells
           :title
           :reference
           :title
           :creator
           :publisher
           :subject
           :description
           :keywords
           :rights
           :version
           :created-on)
  ;; Constructors
  (:export :make-meta
           :make-content
           :make-text
           :make-paragraph
           :make-bold
           :make-italic
           :make-underline
           :make-strikethrough
           :make-code
           :make-superscript
           :make-subscript
           :make-code-block
           :make-inline-quote
           :make-block-quote
           :make-document-link
           :make-web-link
           :make-list-item
           :make-definition
           :make-unordered-list
           :make-ordered-list
           :make-definition-list
           :make-image
           :make-figure
           :make-table
           :make-row
           :make-cell
           :make-section
           :make-document)
  ;; Node definition
  (:export :define-node
           :find-node
           :find-tag
           :find-special-slots)
  (:documentation "CommonDoc classes and and accessors."))

(defpackage common-doc.error
  (:use :cl)
  (:export :common-doc-error
           :macro-error
           :no-macro-expander
           :node)
  (:documentation "CommonDoc errors."))

(defpackage common-doc.file
  (:use :cl)
  (:export :*base-directory*
           :absolute-path
           :relativize-pathname)
  (:documentation "File-related operations for CommonDoc."))

(defpackage common-doc.format
  (:use :cl :common-doc)
  (:export :document-format
           :parse-document
           :emit-document
           :emit-to-string)
  (:documentation "CommonDoc input/output formats."))

(defpackage common-doc.macro
  (:use :cl)
  (:import-from :common-doc
                :content-node
                :document-node
                :base-list
                :definition
                :term
                :document
                :children)
  (:export :macro-node
           :name
           :expand-macro
           :expand-macros)
  (:documentation "CommonDoc macros."))

(defpackage common-doc.util
  (:use :cl :common-doc)
  (:export :string-to-slug)
  (:documentation "CommonDoc utilities."))

(defpackage common-doc.ops
  (:use :cl :common-doc)
  (:export :traverse-document
           :with-document-traversal
           :collect-figures
           :collect-tables
           :collect-external-links
           :collect-all-text
           :fill-unique-refs
           :table-of-contents
           :node-equal
           :node-specific-equal)
  (:documentation "Common operations on CommonDoc documents."))
