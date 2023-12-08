(in-package #:aoc2023/ex8)

(defparameter *current* nil)
(defparameter *moves* nil)
(defparameter *graph* nil)

(defun read-node (line)
  (ppcre:register-groups-bind ((#'read-from-string node left right))
      ("^(\\w+) = \\((\\w+), (\\w+)\\)$" line)
    (list node (cons left right))))

(defun parse-file (file)
  (destructuring-bind ((moves) graph)
      (read-file-as-lines-blocks file)
    (setf moves (coerce moves 'list))
    (setf (cdr (last moves)) moves)     ; circular movements
    (list moves (mapcar 'read-node graph))))

(defun initialize-structures (moves graph)
  (setf *current* 'aaa)
  (setf *moves* moves)
  (setf *graph* (make-hash-table :size (length graph) :test 'eq))
  (dolist (node graph)
    (setf (gethash (first node) *graph*) (second node))))

(defun make-move (dir)
  (let ((next (gethash *current* *graph*)))
    (case dir
      (#\L (setf *current* (car next)))
      (#\R (setf *current* (cdr next))))
    *current*))

(defun answer-ex-8-1 ()
  (apply 'initialize-structures (parse-file "../inputs/input8.txt"))
  (loop :for dir :in *moves*
        :for i :from 0
        :thereis (and (eq *current* 'zzz) i)
        :do (make-move dir)))

;; Part 2

(defun answer-ex-8-2 ())
