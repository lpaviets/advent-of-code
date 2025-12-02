(in-package #:aoc2025/ex1)

(defparameter *input* #P"input")
(defparameter *test* #P"test")

(defun read-rotations (file)
  (read-file-as-lines file
                      :parse (lambda (line)
                               (let ((num (parse-integer line :start 1)))
                                 (if (char= (char line 0) #\R)
                                     num
                                     (- num))))))

(defun make-rotations (rotations)
  (loop :with pos = 50
        :for value :in rotations
        :do (setf pos (mod (+ pos value) 100))
        :count (zerop pos)))

(defun make-rotations-count-0 (rotations)
  (loop :with pos = 50 ;; Always 0 <= pos < 100
        :for value :in rotations
        :for (q r) = (multiple-value-list (truncate value 100)) ; towards 0
        :sum (abs q)
        :count (or (and (plusp pos) (<= (+ pos r) 0)) ; goes "below"
                   (<= 100 (+ pos r)))
        :do (setf pos (mod (+ pos r) 100))))

(defun answer-ex-1-1 (file)
  (make-rotations (read-rotations file)))

(defun answer-ex-1-2 (file)
  (make-rotations-count-0 (read-rotations file)))
