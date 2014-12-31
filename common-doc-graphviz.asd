(defsystem common-doc-graphviz
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :maintainer "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :version "0.1"
  :depends-on (:common-doc
               :trivial-shell)
  :components ((:module "contrib"
                :components
                ((:module "graphviz"
                  :components
                  ((:file "graphviz"))))))
  :description "Graphviz macro for CommonDoc."
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "contrib/graphviz/README.md")))
