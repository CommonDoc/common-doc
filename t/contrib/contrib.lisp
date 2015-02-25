(in-package :cl-user)
(defpackage common-doc-test.contrib
  (:use :cl :fiveam :common-doc)
  (:import-from :common-doc.util
                :doc
                :make-text)
  (:export :tests))
(in-package :common-doc-test.contrib)
