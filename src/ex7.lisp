(in-package #:aoc2021/ex7)

(defun total-fuel (list pos)
  (loop :for x :in list
        :sum (abs (- x pos))))

(defun least-total-fuel (list &optional (fuel-conso-function 'total-fuel))
  (loop :with min = (apply 'min list)
        :with max = (apply 'max list)
        :for pos :from min :upto max
        :minimize (funcall fuel-conso-function list pos)))

;;; Part 2

(defun total-fuel-expensive (list pos)
  (loop :for x :in list
        :for dist = (abs (- x pos))
        :sum (/ (* dist (1+ dist)) 2)))

(defun answer-ex-7-1 ()
  (let ((list (car (read-file-as-lines "inputs/input7.txt"
                                       :parse 'coma-separated-int-line))))
    (least-total-fuel list)))

(defun answer-ex-7-2 ()
  (let ((list (car (read-file-as-lines "inputs/input7.txt"
                                       :parse 'coma-separated-int-line))))
    (least-total-fuel list 'total-fuel-expensive)))
