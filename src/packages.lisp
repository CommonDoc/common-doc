(in-package :cl-user)
(defpackage common-doc
  (:use :cl :trivial-types :anaphora)
  (:export ;; Document classes
           :document-node
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
           :document
           ;; Accessors
           :metadata
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
           :created-on
           ;; Node definition
           :define-node
           :find-node
           :find-tag
           :find-special-slots)
  (:documentation "CommonDoc classes and and accessors."))

(defpackage common-doc.error
  (:use :cl)
  (:export :common-doc-error
           :macro-error
           :no-macro-expander
           :bad-pathname
           :node
           :path-string)
  (:documentation "CommonDoc errors."))

(defpackage common-doc.file
  (:use :cl)
  (:export :*base-directory*
           :absolute-path)
  (:documentation "File-related operations for CommonDoc."))

(defpackage common-doc.ops
  (:use :cl :common-doc)
  (:export :traverse-document
           :collect-figures
           :collect-tables
           :collect-external-links
           :collect-all-text
           :node-equal
           :node-specific-equal)
  (:documentation "Common operations on CommonDoc documents."))

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
                :children)
  (:export :macro-node
           :name
           :expand-macro
           :expand-macros)
  (:documentation "CommonDoc macros."))

(defpackage common-doc.util
  (:use :cl :common-doc)
  (:export :doc
           :make-meta
           :make-text
           :string-to-slug)
  (:documentation "CommonDoc utilities."))
