(defsystem common-doc-split-paragraphs
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :maintainer "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :version "0.1"
  :depends-on (:common-doc
               :cl-ppcre)
  :components ((:module "contrib"
                :components
                ((:module "split-paragraphs"
                  :components
                  ((:file "split-paragraphs"))))))
  :description "Automatically generate paragraphs by splitting text nodes."
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "contrib/split-paragraphs/README.md")))
