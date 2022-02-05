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
