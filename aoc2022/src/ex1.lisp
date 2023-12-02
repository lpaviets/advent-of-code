(in-package #:aoc2022/ex1)

(defun read-elves (file)
  (org.numbra.perso:read-file-as-lines-blocks file
                                              :parse 'parse-integer))

(defun count-calories (elf)
  (reduce #'+ elf))

(defun count-max-calories (elves)
  (reduce #'max elves :key #'count-calories))

(defun count-k-max-calories (elves k)
  (let ((sorted-elves (sort elves #'> :key #'count-calories)))
    (reduce #'+ sorted-elves :end k :key #'count-calories)))

(defun answer-ex-1-1 ()
  (let ((elves (read-elves "../inputs/input1")))
    (count-max-calories elves)))

(defun answer-ex-1-2 ()
  (let ((elves (read-elves "../inputs/input1")))
    (count-k-max-calories elves 3)))
