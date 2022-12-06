(in-package #:aoc2022/ex6)

;;; "Slow" solution (but still about ONE ms on a virtual machine ...)
(defun pos-signal (sig window)
  (loop :for start :below (- (length sig) window)
        :for end = (+ start window)
        :when (apply #'char/= (coerce (subseq sig i end) 'list))
          :do (return end)))

(defun answer-ex-6-1 ()
  (pos-signal (read-file-one-line "../inputs/input6") 4))

(defun answer-ex-6-2 ()
  (pos-signal (read-file-one-line "../inputs/input6") 14))
