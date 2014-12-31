(in-package :cl-user)
(defpackage common-doc
  (:use :cl :trivial-types)
  (:export ;; Document classes
           :<document-node>
           :<content-node>
           :<text-node>
           :<paragraph>
           :<markup>
           :<bold>
           :<italic>
           :<underline>
           :<strikethrough>
           :<code>
           :<superscript>
           :<subscript>
           :<code-block>
           :<quote>
           :<inline-quote>
           :<block-quote>
           :<link>
           :<internal-link>
           :<external-link>
           :<web-link>
           :<list>
           :<list-item>
           :<definition>
           :<unordered-list>
           :<ordered-list>
           :<definition-list>
           :<image>
           :<figure>
           :<table>
           :<row>
           :<cell>
           :<section>
           :<document>
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
           :items
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
  (:documentation "CommonDoc classes and and accessors."))

(defpackage common-doc.ops
  (:use :cl :common-doc)
  (:export :traverse-document
           :collect-figures
           :collect-tables)
  (:documentation "Common operations on CommonDoc documents."))

(defpackage common-doc.format
  (:use :cl :common-doc)
  (:export :<format>
           :parse-document
           :emit-document
           :emit-to-string)
  (:documentation "CommonDoc input/output formats."))

(defpackage common-doc.error
  (:use :cl)
  (:export :<common-doc-error>
           :<macro-error>
           :<no-macro-expander>
           :node)
  (:documentation "CommonDoc errors."))

(defpackage common-doc.macro
  (:use :cl :common-doc)
  (:export :<macro-node>
           :name
           :attributes
           :expand-macro)
  (:documentation "CommonDoc macros."))

(defpackage common-doc.util
  (:use :cl :common-doc)
  (:export :doc)
  (:documentation "CommonDoc utilities."))