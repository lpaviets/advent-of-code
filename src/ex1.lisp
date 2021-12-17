(in-package #:aoc2021/ex1)

(defun count-increased (list)
  (loop :for (a b) :on list
        :count (and b (> b a))))

(defun answer-ex-1-1 ()
  (count-increased
   (read-file-as-integers "../inputs/input1.txt")))

(defun count-mean-increased (list)
  (loop :for (a b c d) :on list
        :count (and d (> (+ b c d)
                         (+ a b c)))))

(defun answer-ex-1-2 ()
  (count-mean-increased
   (read-file-as-integers "../inputs/input1.txt")))
