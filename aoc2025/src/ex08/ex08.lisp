(in-package #:aoc2025/ex8)

(defparameter *input* #P"input")
(defparameter *test* #P"test")

(defun read-boxes (file)
  (read-file-as-lines file :parse 'coma-separated-int-line))

(defun distance (box1 box2)
  (loop :for x1 :in box1
        :for x2 :in box2
        :sum (expt (- x2 x1) 2)))

(defun all-distances (boxes)
  (let ((distances nil))
    (loop :for (box1 . others) :on boxes
          :do (loop :for box2 :in others
                    :do (push (list box1 box2 (distance box1 box2)) distances)))
    (sort distances '< :key 'third)))

(defun compute-circuits (boxes)
  (let* ((distances (all-distances boxes))
         (keep-edges (subseq distances 0 1000))
         (edges-table (make-hash-table :test 'equalp)))
    (loop :for (box1 box2 dist) :in keep-edges
          :do (push (cons box2 dist) (gethash box1 edges-table)))
    (flet ((edges (box)
             (gethash box edges-table)))
      (let ((circuits (connected-components boxes #'edges :test 'equalp)))
        ;; (reduce '* (sort (mapcar 'hash-table-count circuits) '>) :end 3)
        circuits
        ))))

(defun answer-ex-8-1 (file))

(defun answer-ex-8-2 (file))
