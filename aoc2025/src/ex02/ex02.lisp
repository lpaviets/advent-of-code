(in-package #:aoc2025/ex2)

(defparameter *input* #P"input")
(defparameter *test* #P"test")

(defun read-ranges (file)
  (let* ((line (read-file-one-line file))
         (pairs (cl-ppcre:split "," line)))
    (mapcar (lambda (pair)
              (cl-ppcre:register-groups-bind ((#'parse-integer low high))
                  ("(\\d+)-(\\d+)" pair)
                (list low high)))
            pairs)))

(defun duplicate-integer (num times)
  (let* ((n (integer-length-base num))
         (pow (expt 10 n)))
    (loop :repeat times
          :for res = num :then (+ num (* pow res))
          :finally (return res))))

(defun collect-invalid-range-parts (low high k)
  (loop :with nlow = (integer-length-base low)
        :with nhigh = (integer-length-base high)
        ;; Disgusting bounds
        :for part :from (ceiling (expt 10 (1- (truncate nlow k)))) :to (expt 10 (floor nhigh k))
        :for candidate = (duplicate-integer part k)
        :when (<= low candidate high)
          :collect candidate))

(defun collect-invalid-range-all (low high)
  (delete-duplicates (loop :for k :from 2 :to (integer-length-base high)
                           :append (collect-invalid-range-parts low high k))))

(defun answer-ex-2-1 (file)
  (let ((ranges (read-ranges file)))
    (loop :for (low high) :in ranges
          :sum (reduce '+ (collect-invalid-range-parts low high 2)))))

(defun answer-ex-2-2 (file)
  (let ((ranges (read-ranges file)))
    (loop :for (low high) :in ranges
          :sum (reduce '+ (collect-invalid-range-all low high)))))
