;;;; package.lisp
;;
;;;; Copyright (c) 2022 S. Karthik Kumar <karthikkumar.s@protonmail.com>


(defpackage #:cl-dist-measure
  (:use #:cl
        #:iterate
        #:alexandria-2)
  (:shadowing-import-from #:alexandria-2 #:flatten))


(in-package #:cl-dist-measure)
