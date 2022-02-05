;;;; package.lisp
;;
;;;; Copyright (c) 2022 S. Karthik Kumar <karthikkumar.s@protonmail.com>


(defpackage #:cl-dist-measure
  (:use #:cl
        #:iterate
        #:alexandria-2)
  (:export
   #:*standard-optimize-settings*
   #:*full-optimize-settings*
   #:*full-debug-settings*)
  (:shadowing-import-from #:alexandria-2 #:flatten))
