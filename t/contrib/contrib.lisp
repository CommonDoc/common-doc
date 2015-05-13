(in-package :cl-user)
(defpackage common-doc-test.contrib
  (:use :cl :fiveam :common-doc)
  (:export :contrib))
(in-package :common-doc-test.contrib)

(def-suite contrib)
(in-suite contrib)
