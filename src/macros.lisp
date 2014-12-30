(in-package :common-doc)

(defclass <macro-node> (<content-node>)
  ((name :accessor name
         :initarg :name
         :type string
         :documentation "The name of the macro.")
   (attributes :accessor attributes
               :initarg :attributes
               :type hash-table
               :documentation "Macro attributes."))
  (:documentation "A macro to be expanded."))

(defgeneric expand-macro (node)
  (:documentation "Replace a macro node with a primitive node."))
