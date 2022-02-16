(in-package #:cl-dist-measure)


(eval-when (:compile-toplevel :load-toplevel)
  (defparameter *standard-optimize-settings*
    '(optimize
      (speed 3)
      (safety 1)
      (debug 1)
      (compilation-speed 0))
    "Normal optimization options."))


(eval-when (:compile-toplevel :load-toplevel)
  (defparameter *full-optimize-settings*
    '(optimize
      (speed 3)
      (safety 0)
      (debug 0)
      (compilation-speed 0))
    "Option to fully optimize the program."))

(eval-when (:compile-toplevel :load-toplevel)
  (defparameter *full-debug-settings*
    '(optimize
      (speed 0)
      (safety 3)
      (debug 3)
      (space 0)
      (compilation-speed 0))
    "Option to fully debug the program."))


(deftype single-float-vector ()
  `(simple-vector single-float (*)))

(deftype single-float-array ()
  `(simple-array single-float (*)))


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


(defun vectors-with-same-sizep (first-vec second-vec)
  "Predicate function to compare two input vectors for the size."
  (declare #.*full-optimize-settings*)
  (declare (type (simple-vector *) first-vec second-vec))
  (equalp (array-total-size first-vec)
          (array-total-size second-vec)))
