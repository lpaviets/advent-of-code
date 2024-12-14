(in-package #:aoc2024/ex14)

(defparameter *input* #P"input")
(defparameter *test* #P"test")

;; Painful: arrays are organised/printed with HEIGHT/WIDTH rather than the
;; standard directions for the axis (where the horizontal one comes first)

(defun parse-bot (line)
  (destructuring-bind (px py vx vy) (collect-integers-in-line line)
    (list (list py px) (list vy vx))))

(defun initial-conditions (file)
  (read-file-as-lines file :parse 'parse-bot))

(defun pos-after-n-steps (init-pos velocity steps width height)
  (destructuring-bind (py px) init-pos
    (destructuring-bind (vy vx) velocity
      (let ((end-x (mod (+ px (* steps vx)) width))
            (end-y (mod (+ py (* steps vy)) height)))
        `(,end-y ,end-x)))))

(defun find-quadrant (pos width height)
  (let ((half-width (truncate width 2))
        (half-height (truncate height 2))
        (quadrant 0))
    (destructuring-bind (y x) pos
      (when (< half-width x)
        (incf quadrant 1))
      (when (< half-height y)
        (incf quadrant 2))
      (unless (or (= x half-width)
                  (= y half-height))
        quadrant))))

;; P2: stupid
(defun make-step (robot width height)
  (destructuring-bind ((py px) (vy vx)) robot
    (setf (first (first robot)) (mod (+ py vy) height)
          (second (first robot)) (mod (+ px vx) width))))

(defun make-step-all (robots width height)
  (dolist (robot robots robots)
    (make-step robot width height)))

(defun dispersion-score (grid)
  (let ((count 0))
    (do-array (i j x grid)
      (when x (incf count)))
    count))

(defun count-consecutive-row (grid row)
  (loop :for col :below (array-dimension grid 1)
        :for cell = (aref grid row col)
        :for current = 0 :then (if cell (1+ current) 0)
        :maximize current))

(defun count-consecutive-col (grid col)
  (loop :for row :below (array-dimension grid 0)
        :for cell = (aref grid row col)
        :for current = 0 :then (if cell (1+ current) 0)
        :maximize current))

(defun consecutive-score (grid)
  (+ (loop :for row :below (array-dimension grid 0)
           :maximize (count-consecutive-row grid row))
     (loop :for col :below (array-dimension grid 1)
           :maximize (count-consecutive-col grid col))))

(defun score (grid)
  (+  (dispersion-score grid)
      (* 10 (consecutive-score grid))))

(defun sort-interesting-iterations (robots width height iterations-max)
  (sort (loop :with grid = (make-array (list height width) :initial-element nil)
              :for i :below iterations-max
              :do (loop :for ((py px) vel) :in robots
                        :do (setf (aref grid py px) t))
              :collect (cons i (score grid))
              :do (loop :for ((py px) vel) :in robots
                        :do (setf (aref grid py px) nil))
              :do (make-step-all robots width height))
        '> :key 'cdr))

(defun answer-ex-14-1 (file)
  (loop :with (width height) = '(101 103)
        ;; '(11 7)
        :with quadrants = (make-array 4 :initial-element 0)
        :for (init-pos velocity) :in (initial-conditions file)
        :for end-pos = (pos-after-n-steps init-pos velocity 100 width height)
        :for quadrant = (find-quadrant end-pos width height)
        :when quadrant
          :do (incf (aref quadrants quadrant))
        :finally (return (reduce '* (print quadrants)))))

(defun plot-robots (robots width height)
  (let ((grid (make-array (list height width) :initial-element #\Space)))
    (loop :for ((py px) vel) :in robots
          :do (setf (aref grid py px) #\#))
    (print-array grid)
    nil))

(defun answer-ex-14-2 (file &optional (to-consider 1) (max-steps 10000))
  (let* ((width 101)
         (height 103)
         (robots (initial-conditions file))
         (stored-robots (copy-tree robots))
         (interesting (sort-interesting-iterations robots
                                                   width height
                                                   max-steps))
         (max-interesting-iter (reduce 'max interesting
                                       :key 'car
                                       :end to-consider)))
    (loop :for i :to max-interesting-iter
          :for (iter . score) = (find i interesting :end to-consider :key 'car)
          :when iter
            :do (format t "~
----------------------------~
~%~%Iteration ~A: score = ~A
----------------------------~%~%" i score)
                (plot-robots stored-robots width height)
          :do (make-step-all stored-robots width height))))
