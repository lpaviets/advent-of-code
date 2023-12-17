(in-package #:aoc2023/ex17)

(defun next-pos (pos dir)
  (destructuring-bind (x y) pos
    (ecase dir
      (:up    (list (1- x) y))
      (:down  (list (1+ x) y))
      (:left  (list x (1- y)))
      (:right (list x (1+ y))))))

(defun switch-dirs (dir pos grid)
  (loop :for d :in (case dir
                     ((:left :right) '(:down :up))
                     ((:down :up) '(:left :right)))
        :for next-pos = (next-pos pos d)
        :when (apply #'array-in-bounds-p grid next-pos)
          :collect (list next-pos d 1)))

(defun next-possible-pos (pos dir consecutive grid)
  (if (and (< consecutive 3)
           (apply #'array-in-bounds-p grid (next-pos pos dir)))
      (cons (list (next-pos pos dir) dir (1+ consecutive)) (switch-dirs dir pos grid))
      (switch-dirs dir pos grid)))

(defun next-possible-pos-ultra (pos dir consecutive grid)
  (cond
    ((= consecutive 10) (switch-dirs dir pos grid)) ; forced to change
    ((apply #'array-in-bounds-p grid (next-pos pos dir)) ; can continue, and maybe can change
     (if (< consecutive 4)
         (list (list (next-pos pos dir) dir (1+ consecutive))) ; can't change in fact, so must continue
         (cons (list (next-pos pos dir) dir (1+ consecutive)) ; do whatever
               (switch-dirs dir pos grid))))
    ((< consecutive 4) nil)             ; must continue but can't
    (t (switch-dirs dir pos grid)))) ; must change


(defun plot-path (final parents grid)
  (let ((path (make-array (array-dimensions grid)
                          :initial-element 0)))
    (loop :for vertex = final :then (gethash vertex parents)
          :for (x y) = (car vertex)
          :while vertex
          :do (setf (aref path x y) 1)
          :finally (print-array path))))

(defun lowest-heat-path (grid &key ultra)
  (let* ((goal (mapcar '1- (array-dimensions grid)))
         (source (list (list 0 0) :right 0))
         (source-ultra (list (list 0 0) :down 0))
         (correction (- (apply #'aref grid goal)
                        (apply #'aref grid (car source)))))
    (flet ((edges (vertex)
             (destructuring-bind (pos dir consecutive) vertex
               (loop :with weight = (apply #'aref grid (car vertex))
                     :for next :in (if ultra
                                       (next-possible-pos-ultra pos dir consecutive grid)
                                       (next-possible-pos pos dir consecutive grid))
                     :collect (cons next weight))))
           (target (vertex)
             (if ultra
                 (and (equalp goal (first vertex)) (<= 4 (third vertex)))
                 (equalp goal (first vertex)))))
      (+ correction
         (if ultra
             (min (shortest-path #'edges source #'target :test 'equalp)
                  (shortest-path #'edges source-ultra #'target :test 'equalp))
             (shortest-path #'edges source #'target :test 'equalp))))))

(defun answer-ex-17-1 ()
  (let ((grid (read-file-as-array "../inputs/input17.txt"
                                  :as-digits t)))
    (lowest-heat-path grid)))

(defun answer-ex-17-2 ()
  (let ((grid (read-file-as-array "../inputs/input17.txt"
                                  :as-digits t)))
    (lowest-heat-path grid :ultra t)))
