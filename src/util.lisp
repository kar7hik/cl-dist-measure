(in-package #:cl-dist-measure)


(defvar *standard-optimize-settings*
  '(optimize
    (speed 3)
    (safety 1)
    (debug 1)
    (compilation-speed 0))
  "Normal optimization options.")

(defvar *full-optimize-settings*
  '(optimize
    (speed 3)
    (safety 0)
    (debug 0)
    (compilation-speed 0))
  "Option to fully optimize the program.")

(defvar *full-debug-settings*
  '(optimize
    (speed 0)
    (safety 3)
    (debug 3)
    (space 0)
    (compilation-speed 0))
  "Option to fully debug the program.")


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
