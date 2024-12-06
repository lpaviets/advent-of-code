(in-package #:aoc2024/ex6)

(defparameter *input* #P"input")
(defparameter *test* #P"test")

(defun parse-grid (file)
  (read-file-as-array file))

(defun start-pos (grid)
  (do-array (i j x grid)
    (when (char= #\^ x)
      (setf (aref grid i j) #\.)
      (return-from start-pos (list i j)))))

(defun grid-turn-right (dir)
  (ecase dir
    (:up    :right)
    (:down  :left)
    (:left  :up)
    (:right :down)))

(defun dir-to-num (dir)
  (ecase dir
    (:up    0)
    (:down  1)
    (:left  2)
    (:right 3)))

(defun grid-free-cell-p (pos grid)
  (or (not (grid-valid-pos-p pos grid))
      (char= (grid-at pos grid) #\.)))

(defun visited-cells-to-exit (start-pos grid)
  (let ((visited (make-array (array-dimensions grid) :initial-element nil))
        (dir :up)
        (count 0)
        (pos start-pos))
    (loop :while (grid-valid-pos-p pos grid)
          :unless (aref visited (first pos) (second pos))
            :do (incf count)
          :do (setf (aref visited (first pos) (second pos)) t)
              (let ((next (grid-pos-in-direction pos dir)))
                (if (grid-free-cell-p next grid)
                    (setf pos next)
                    (setf dir (grid-turn-right dir)))))
    count))

(defun detect-loop (start-pos grid)
  (let ((visited (make-array (cons 4 (array-dimensions grid)) :initial-element nil)))
    (loop :for pos = start-pos :then (if next-free-p try-next pos)
          :for dir = :up :then (if next-free-p dir (grid-turn-right dir))
          :for (x y) = pos
          :for try-next = (grid-pos-in-direction pos dir)
          :for next-free-p = (grid-free-cell-p try-next grid)
          :for dir-to-num = (dir-to-num dir)
          :while (grid-valid-pos-p pos grid)
          :if (aref visited dir-to-num x y)
            :do (return-from detect-loop t)
          :else
            :do (setf (aref visited dir-to-num x y) t))
    nil))

(defun loop-with-obstacle-p (start-pos obstacle-pos grid)
  (let ((old (grid-at obstacle-pos grid)))
    (setf (grid-at obstacle-pos grid) #\#)
    (prog1
        (detect-loop start-pos grid)
      (setf (grid-at obstacle-pos grid) old))))

(defun answer-ex-6-1 (file)
  (let* ((grid (parse-grid file))
         (start-pos (start-pos grid)))
    (visited-cells-to-exit start-pos grid)))

(defun answer-ex-6-2 (file)
  (let* ((grid (parse-grid file))
         (start-pos (start-pos grid))
         (count 0))
    (do-array (i j x grid count)
      (let ((pos (list i j)))
        (when (and (grid-free-cell-p pos grid)
                   (not (equalp pos start-pos))
                   (loop-with-obstacle-p start-pos pos grid))
          (incf count))))))
