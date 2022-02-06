;;;; cl-dist-measure.lisp
;;
;;;; Copyright (c) 2022 S. Karthik Kumar <karthikkumar.s@protonmail.com>


(in-package #:cl-dist-measure)

(defparameter *generator* (random-state:make-generator :mersenne-twister-64 123)
  "Random state generator.")
(defparameter *low* 0
  "Low limit for random number generator.")
(defparameter *high* 1
  "High limit for random number generator.")


(defun vector-euclidean-distance (x-vector y-vector)
  "Returns the euclidean distance between x-vector and y-vector."
  ;; (declare #.*full-optimize-settings*)
  (declare (type (simple-vector *) x-vector y-vector))
  (let ((xvec-size (array-total-size x-vector)))
    (declare (type fixnum xvec-size))
    (unless (vectors-with-same-sizep)
      (error "~&Length of both input vectors must be same."))
    (iterate (declare (iterate:declare-variables))
      (for i :from 0 :below xvec-size)
      (collect (square (- (row-major-aref x-vector i)
                          (row-major-aref y-vector i))) into result result-type 'simple-vector)
      (finally (return (sqrt (reduce #'+ result)))))))


;; (defparameter *x* (create-random-vector 20))
;; (defparameter *y* (create-random-vector 20))

;; (time (vector-euclidean-distance *x* *y*))
