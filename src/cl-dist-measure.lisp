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


(defun euclidean-distance (x-vector y-vector)
  "Returns the euclidean distance between x-vector and y-vector."
  ;; (declare #.*full-optimize-settings*)
  (declare (type single-float-vector x-vector y-vector))
  (let ((xvec-size (array-total-size x-vector)))
    (declare (type fixnum xvec-size))
    (unless (vectors-with-same-sizep x-vector y-vector)
      (error "~&Length of both input vectors must be same."))
    (iterate (declare (iterate:declare-variables))
      (for i :from 0 :below xvec-size)
      (collect (square (- (row-major-aref x-vector i)
                          (row-major-aref y-vector i))) into result result-type 'simple-vector)
      (finally (return (sqrt (reduce #'+ result)))))))


(defun cosine-similarity (x-vector y-vector &key (n (length x-vector)))
  "Returns the Cosine Similarity between two vectors."
  (declare (type single-float-vector  x-vector y-vector)
           (type fixnum n))
  (let ((numerator 0d0)
        (denom-1 0d0)
        (denom-2 0d0))
    (declare (type float numerator denom-1 denom-2))
    (let ((temp-a 0d0)
          (temp-b 0d0))
      (declare (type float temp-a temp-b))
      (iterate
        (for i :below n)
        (setf temp-a (aref x-vector i))
        (setf temp-b (aref y-vector i))
        (incf numerator (* temp-a temp-b))
        (incf denom-1 (* temp-a temp-a))
        (incf denom-2 (* temp-b temp-b)))
      (setf denom-1 (sqrt denom-1))
      (setf denom-2 (sqrt denom-2))
      (if (or (zerop denom-1)
              (zerop denom-2))
          0d0
          (/ numerator denom-1 denom-2)))))


;; (defparameter *x* (create-random-vector 20))
;; (defparameter *y* (create-random-vector 20))

;; (time (euclidean-distance *x* *y*))
;; (time (cosine-similarity *x* *y*))
