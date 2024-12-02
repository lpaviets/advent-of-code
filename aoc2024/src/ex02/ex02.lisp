(in-package #:aoc2024/ex2)

(defparameter *input* #P"input")
(defparameter *test* #P"test")

(defun parse-file (file)
  (read-file-as-sexprs file))

(defun validate-row (row)
  (and (or (apply '< row)
           (apply '> row))
       (loop :for (x y) :on row
             :while y
             :always (<= (abs (- x y)) 3))))

;; Part 2
(defun remove-level (row level)
  (loop :for x :in row
        :for i :from 0
        :unless (= i level) :collect x))

(defun validate-ignoring-level (row i)
  (let ((row-ignored (remove-level row i)))
    (and (or (apply '< row-ignored)
             (apply '> row-ignored))
         (loop :for (x y) :on row-ignored
               :while y
               :always (<= (abs (- x y)) 3)))))

(defun validate-with-errors (row)
  (loop :for i :from -1 :below (length row) ; start at -1: don't remove anything
        :thereis (validate-ignoring-level row i)))

(defun answer-ex-2-1 (file)
  (count-if 'validate-row (parse-file file)))

(defun answer-ex-2-2 (file)
  (count-if 'validate-with-errors (parse-file file)))
