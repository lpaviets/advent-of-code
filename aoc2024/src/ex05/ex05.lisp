(in-package #:aoc2024/ex5)

(defparameter *input* #P"input")
(defparameter *test* #P"test")

(defun parse-file (file)
  (read-file-as-lines-blocks file :parse 'collect-integers-in-line))

(defun valid-update-for-rule-p (update rule)
  (destructuring-bind (first last) rule
    (let ((x (position first update))
          (y (position last update)))
      (or (not x)
          (not y)
          (< x y)))))

(defun valid-update-p (update rules)
  (every (lambda (rule)
           (valid-update-for-rule-p update rule))
         rules))

(defun make-edges (rules update)
  (let ((edges (make-array 100 :initial-element nil)))
    (loop :for (x y) :in rules
          :when (and (member x update)
                    (member y update))
            :do (push (cons y 1) (aref edges x)))
    edges))

(defun sort-pages (rules update)
  (let ((edges (make-edges rules update)))
    (flet ((edges-from (page)
             (aref edges page)))
      (topological-sort #'edges-from update :multi-sources t))))

(defun answer-ex-5-1 (file)
  (loop :with (rules updates) = (parse-file file)
        :for update :in updates
        :when (valid-update-p update rules)
          :sum (nth (truncate (length update) 2) update)))

(defun answer-ex-5-2 (file)
  (loop :with (rules updates) = (parse-file file)
        :for update :in updates
        :unless (valid-update-p update rules)
          :sum (let* ((sorted (sort-pages rules update))
                      (sorted-update (remove-if-not (lambda (v)
                                                      (member v update))
                                                    sorted)))
                 (nth (truncate (length update) 2) sorted-update))))
