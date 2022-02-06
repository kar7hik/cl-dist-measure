;;;; cl-dist-measure.asd
;;
;;;; Copyright (c) 2022 S. Karthik Kumar <karthikkumar.s@protonmail.com>


(asdf:defsystem #:cl-dist-measure
  :description "Describe cl-dist-measure here"
  :author "S. Karthik Kumar <karthikkumar.s@protonmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:iterate #:alexandria #:random-state)
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "utils")
                             (:file "cl-dist-measure")))))
