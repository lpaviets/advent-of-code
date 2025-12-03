(in-package #:aoc2025/ex3)

(defparameter *input* #P"input")
(defparameter *test* #P"test")

(defun read-banks (file)
  (read-file-as-lines file
                      :parse (lambda (line)
                               (map 'vector 'digit-char-p line))))

(defun find-max-pair (bank)
  (let* ((n (length bank))
         (pos-max (car (nth-value 1 (argmax bank))))
         (pos-2nd-max (if (= pos-max (1- n))
                          (car (nth-value 1 (argmax bank :end (1- n))))
                          (car (nth-value 1 (argmax bank :start (1+ pos-max)))))))
    (when (< pos-2nd-max pos-max)
      (rotatef pos-2nd-max pos-max))
    (+ (* 10 (aref bank pos-max)) (aref bank pos-2nd-max))))


;; Dynamic programming : A[i][j] : in a given bank, what is the best joltage
;; using j batteries among the batteries 0...i ?

(defun find-best-joltage (bank &optional (to-flip 12))
  (let* ((n (length bank))
         (best (make-array (list n (1+ to-flip)) :initial-element 0)))
    ;; A[i+1][j] = max(A[i][j], merge(A[i][j-1], bank[i+1]))
    ;; A[i][j+1] = max(A[i-1][j+1], merge(A[i-1][j], bank[i]))

    ;; Only non-zero init required : one battery, the first one
    (setf (aref best 0 1) (aref bank 0))
    ;; Loop to fill the array
    (loop :for j :from 1 :to to-flip :do
      (loop :for i :from 1 :below n :do
        (setf (aref best i j)
              (max (aref best (1- i) j)
                   (+ (* 10 (aref best (1- i) (1- j)))
                      (aref bank i))))))
    (aref best (1- n) to-flip)))

(defun answer-ex-3-1 (file)
  (reduce '+ (read-banks file) :key 'find-max-pair))

(defun answer-ex-3-2 (file)
  (reduce '+ (read-banks file) :key 'find-best-joltage))
