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


(defun square (value)
  "Returns the square of the number."
  (unless (numberp value)
    (error "The input value must be a number. The type of the input value is ~A.~&" (type-of value)))
  (the number (* value value)))


(defun create-random-vector (vector-len &key (generator *generator*)
                                          (low *low*)
                                          (high *high*)
                                          (type 'double-float))
  "Returns a random vector with length of vector-len.
generator - random-state:make-generator object
low - minimum value
high - maximum value
type - FIXNUM or DOUBLE-FLOAT"
  (declare (type fixnum vector-len))
  (with-gensyms (random-vector)
    (ecase type
      (fixnum (progn
                (setf random-vector
                      (make-array vector-len :initial-element 0))
                (iter
                  (for  i :from 0 :below vector-len)
                  (setf (row-major-aref random-vector i)
                        (random-state:random-int generator low high))
                  (finally (return random-vector)))))
      (double-float (progn
                      (setf random-vector
                            (make-array vector-len :initial-element 0.0d0))
                      (iter
                        (for  i :from 0 :below vector-len)
                        (setf (row-major-aref random-vector i)
                              (random-state:random-float generator low high))
                        (finally (return random-vector))))))))


(defun create-vector-with-valu (args)
  ""
  )


(defun vectors-with-same-sizep (first-vec second-vec)
  "Predicate function to compare two input vectors for the size."
  (declare #.*full-optimize-settings*
           (type (simple-vector *) first-vec second-vec))
  (equalp (array-total-size first-vec)
          (array-total-size second-vec)))


(defun create-vector-with-input-value (value reference-vector)
  "Creates a vector with the given value and same size as reference-vector.
Example use:
(create-vector-with-input-value #(1 2 3) #(2 3 4 5))
;;; #(#(1 2 3) #(1 2 3) #(1 2 3) #(1 2 3))

(create-vector-with-input-value 5 #(2 3 4 5))
;;; #(5 5 5 5)
"
  (declare #.*full-optimize-settings*
           (type (simple-vector *) reference-vector))
  (when (vectors-with-same-sizep ))
  (make-array (array-total-size reference-vector)
              :element-type 'simple-vector
              :initial-element value))


(defun create-vector-with-value (x reference-vector)
  "Returns a vector with value x and same size as reference-vector.
example use:"
  (declare #.*full-optimize-settings*
           (type (simple-vector *) reference-vector))
  (make-array (array-total-size reference-vector)
              :initial-contents x))

(create-vector-with-value #(1 2 3) *y*)


(defun vector-euclidean-distance (x-vector y-vector)
  "Returns the euclidean distance between x-vector and y-vector."
  ;; (declare #.*full-optimize-settings*)
  (declare (type (simple-vector *) x-vector y-vector))
  (let ((xvec-size (array-total-size x-vector))
        (yvec-size (array-total-size y-vector)))
    (declare (type fixnum xvec-size yvec-size))
    (unless (>= xvec-size yvec-size)
      (error "~&~A length must be greater than or equal to the ~A ~%~A < ~A"
             "x-vector"
             "y-vector"
             x-vector
             y-vector))
    (iterate (declare (iterate:declare-variables))
      (for i :from 0 :below xvec-size)
      (collect (square (- (row-major-aref x-vector i)
                          (row-major-aref y-vector i))) into result result-type 'simple-vector)
      (finally (return (sqrt (reduce #'+ result)))))))


(defparameter *x* (create-random-vector 20))
(defparameter *y* (create-random-vector 20))

(time (vector-euclidean-distance *x* *y*))
