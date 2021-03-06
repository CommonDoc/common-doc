(defsystem common-doc-include
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :maintainer "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :version "0.1"
  :depends-on (:common-doc
               :split-sequence)
  :components ((:module "contrib"
                :components
                ((:module "include"
                  :components
                  ((:file "include"))))))
  :description "Including external files into CommonDoc documents."
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "contrib/include/README.md")))
