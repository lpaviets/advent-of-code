(in-package #:aoc2025/ex5)

(defparameter *input* #P"input")
(defparameter *test* #P"test")

;; Stupid P1
(defun read-intervals (file)
  (destructuring-bind (ranges ids)
      (read-file-as-lines-blocks file)
    (list (mapcar (lambda (range)
                    (destructuring-bind (low high)
                        (mapcar 'parse-integer (cl-ppcre:split "-" range))
                      (make-interval low high)))
                  ranges)
          (mapcar 'parse-integer ids))))

(defun freshp (id ranges)
  (loop :for interval :in ranges
          :thereis (interval-contains-p interval id)))

(defun range-to-interval (range)
  (make-interval (first range) (second range)))

(defun answer-ex-5-1 (file)
  (destructuring-bind (ranges ids)
      (read-intervals file)
    (loop :for id :in ids
          :count (freshp id ranges))))

(defun answer-ex-5-2 (file)
  (let ((intervals (car (read-intervals file))))
    (reduce '+ (apply 'interval-union intervals)
            :key 'interval-cardinal)))
