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
         (edges-table (make-hash-table :test 'equal)))
    (loop :for (box1 box2 dist) :in keep-edges
          :do (push (cons box2 dist) (gethash box1 edges-table))
              (push (cons box1 dist) (gethash box2 edges-table)))
    (flet ((edges (box)
             (gethash box edges-table)))
      (connected-components boxes #'edges :test 'equal))))

(defun build-connected-circuits (boxes)
  (let ((distances (all-distances boxes))
        (circuits (uf-initialize boxes :test 'equal :strategy :size)))
    ;; Distances are already sorted
    (loop :while (< 1 (hash-table-count (uf-representatives circuits)))
          :for (box1 box2 distance) :in distances
          :do (uf-union circuits box1 box2)
          :finally (return (* (car box1) (car box2))))))

(defun answer-ex-8-1 (file)
  (let* ((boxes (read-boxes file))
         (circuits (compute-circuits boxes))
         (circuits-size (mapcar 'hash-table-count circuits)))
    (reduce '* (sort circuits-size '>) :end 3)))

(defun answer-ex-8-2 (file)
  (build-connected-circuits (read-boxes file)))
