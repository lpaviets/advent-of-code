;;; Advent of code:
;;; Printing

(in-package #:advent-of-code)

(defun print-array (array)
  (loop :for i :below (array-dimension array 0) :do
    (loop :for j :below (array-dimension array 1) :do
      (format t "~a" (aref array i j)))
    (format t "~%"))
  array)

(defun print-hash (object &optional (stream t))
  (format stream "#HASH{~{~{(~s : ~s)~}~%~^ ~}}"
          (loop :for key :being :the :hash-keys :of object
                  :using (:hash-value value)
                :collect (list key value))))
