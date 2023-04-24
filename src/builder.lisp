(in-package :common-doc.builder)

;;; Interface

(defclass builder ()
  ((document :accessor builder-document
             :initarg :document
             :type document
             :documentation "The internal document.")
   (stack :accessor builder-stack
          :initform (list)
          :type (proper-list (or document-node keyword))
          :documentation "The stack of nodes."))
  (:documentation "A document builder."))

(defun make-builder (document)
  "Create a builder from a document."
  (make-instance 'builder
                 :document document))

(defgeneric add-node (builder node)
  (:documentation "Add a document node to the builder."))

(defgeneric end-node (builder)
  (:documentation "Stop adding children to a node."))

(defgeneric build (builder)
  (:documentation "Return the complete document."))

;;; Implementation

(defmethod add-node ((builder builder) node)
  "Add any node to a builder."
  (push node (builder-stack builder)))

(defmethod end-node ((builder builder) node)
  "End a node."
  (push :end-node (builder-stack builder)))

(defun build-document (document stack)
  "Add the stack into document children."
  (labels ((recur (stack)
             (let ((first (first stack)))
               (if (keywordp first)
                   ;; Command
                   nil
                   ;; Node
                   ))))
    (setf (children document) (recur stack))
    document))

(defmethod build ((builder builder) node)
  "Build the document and return it."
  (build-document (builder-document builder)
                  (builder-stack builder)))

;;; Macros
