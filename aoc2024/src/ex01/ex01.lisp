(in-package #:aoc2024/ex1)

(defparameter *input* "input")
(defparameter *test* "test")

(defun parse-file (file)
  (let ((columns (read-file-as-sexprs file)))
    (apply 'mapcar 'list columns))) ; this transposes

(defun count-occurrences (list &optional (test 'eql))
  (let ((table (make-hash-table :test test)))
    (dolist (x list table)
      (incf (gethash x table 0)))))

(defun answer-ex-1-1 (file)
  (loop :with (left right) = (parse-file file)
        :for x :in (sort left '<)
        :for y :in (sort right '<)
        :sum (abs (- x y))))

(defun answer-ex-1-2 (file)
  (loop :with (left right) = (parse-file file)
        :with occurrences = (count-occurrences right)
        :for x :in left
        :sum (* x (gethash x occurrences 0))))

;;;; Easier but slower solution, works as the file is small
;; (defun answer-ex-1-2 (file)
;;   (destructuring-bind (left right) (parse-file file)
;;     (reduce '+ left :key (lambda (x) (* x (count x right))))))
