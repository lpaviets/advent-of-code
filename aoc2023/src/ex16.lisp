(in-package #:aoc2023/ex16)

(defun next-pos (pos dir)
  (destructuring-bind (x y) pos
    (ecase dir
      (:up    (list (1- x) y))
      (:down  (list (1+ x) y))
      (:left  (list x (1- y)))
      (:right (list x (1+ y))))))

(defun %next-dir (dir tile)
  (case tile
    (#\. (list dir))
    (#\- (case dir
           ((:up :down) (list :left :right))
           (t (list dir))))
    (#\| (case dir
           ((:left :right) (list :up :down))
           (t (list dir))))
    (#\\ (list (ecase dir
                 (:up :left)
                 (:down :right)
                 (:left :up)
                 (:right :down))))
    (#\/ (list (ecase dir
                 (:up :right)
                 (:down :left)
                 (:right :up)
                 (:left :down))))))

(defun next-dir (pos dir grid)
  (when (and pos (apply 'array-in-bounds-p grid pos))
    (let ((tile (apply 'aref grid pos)))
      (%next-dir dir tile))))

(defun beam (start start-dir grid)
  (let ((cache (make-hash-table :test 'equal))
        (beams (list (cons start start-dir))))
    (flet ((update-cache-beams (pos dir)
             (let ((in-cache (gethash pos cache)))
               (unless (member dir in-cache)
                 (push dir (gethash pos cache))
                 (push (cons pos dir) beams)))))
      (loop :while beams
            :for (beam-pos . beam-dir) = (pop beams)
            :for next-pos = (next-pos beam-pos beam-dir)
            :for next-dir = (next-dir next-pos beam-dir grid)
            :do (dolist (dir next-dir)
                  (update-cache-beams next-pos dir))))
    (hash-table-count cache)))

(defun answer-ex-16-1 ()
  (let ((grid (read-file-as-array "../inputs/input16.txt")))
    (beam '(0 -1) :right grid)))

;; No clever cache, just bruteforce all positions !
(defun answer-ex-16-2 ()
  (let ((grid (read-file-as-array "../inputs/input16.txt")))
    (let* ((height (array-dimension grid 0))
           (width (array-dimension grid 1)))
      (max (loop :for j :from 0 :below width
                 :maximize (max (beam (list -1 j) :down grid)
                                (beam (list height j) :up grid)))
           (loop :for i :from 0 :below height
                 :maximize (max (beam (list i -1) :right grid)
                                (beam (list i width) :left grid)))))))
