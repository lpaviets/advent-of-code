(in-package #:aoc2024/ex10)

(defparameter *input* #P"input")
(defparameter *test* #P"test")

(defun read-map (file)
  (read-file-as-array file :as-digits t))

(defun trails-starts (map)
  (let ((start nil))
    (do-array (i j x map start)
      (when (zerop x)
        (push (list i j) start)))))

(defun trails-ends (map)
  (let ((start nil))
    (do-array (i j x map start)
      (when (= 9 x)
        (push (list i j) start)))))

(defun map-neighbours (pos map)
  (loop :with self = (grid-at pos map)
        :for neighbour :in (grid-neighbours pos map)
        :for neighbour-val = (grid-at neighbour map)
        :when (= neighbour-val (1+ self))
          :collect neighbour))

(defun make-edges (map)
  (let ((grid (make-array (array-dimensions map))))
    (do-array (i j x map)
      (setf (aref grid i j)
            (loop :for nghb :in (map-neighbours (list i j) map)
                  :collect (cons nghb 1))))
    (lambda (pos)
      (aref grid (first pos) (second pos)))))

(defun trailhead-score (start ends map)
  (let ((distances (nth-value 1 (bfs (make-edges map) start :test 'equalp)))
        (score 0))
    (dolist (pos ends)
      (when (= 9 (gethash pos distances 0))
        (incf score)))
    score))

;;; Part 2
(defun trailhead-distinct-score (pos map cache)
  (if (= 9 (grid-at pos map))
      1
      (or (grid-at pos cache)
          (setf (grid-at pos cache)
                (loop :for nghb :in (map-neighbours pos map)
                      :sum (trailhead-distinct-score nghb map cache))))))

(defun answer-ex-10-1 (file)
  (let* ((map (read-map file))
         (starts (trails-starts map))
         (ends (trails-ends map)))
    (loop :for start :in starts
          :sum (trailhead-score start ends map))))

(defun answer-ex-10-2 (file)
  (let* ((map (read-map file))
         (starts (trails-starts map))
         (cache (make-array (array-dimensions map) :initial-element nil)))
    (loop :for start :in starts
          :sum (trailhead-distinct-score start map cache))))
