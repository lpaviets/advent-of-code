(in-package #:aoc2023/ex11)

(defun empty-row-p (i grid)
  (loop :for j :below (array-dimension grid 1)
        :always (char= #\. (aref grid i j))))

(defun empty-column-p (j grid)
  (loop :for i :below (array-dimension grid 0)
        :always (char= #\. (aref grid i j))))

(defun empty-rows (grid)
  (loop :with max = (array-dimension grid 0)
        :with acc = nil
        :for i :below max
        :when (empty-row-p i grid)
          :do (push i acc)
        :finally (return (reverse (cons max acc)))))

(defun empty-columns (grid)
  (loop :with max = (array-dimension grid 1)
        :with acc = nil
        :for j :below max
        :when (empty-column-p j grid)
          :do (push j acc)
        :finally (return (reverse (cons max acc)))))

(defparameter *expansion-factor* 2)

(defun coordinate-expanded (coords empty-rows empty-columns)
  (destructuring-bind (x y) coords
    (list (+ x (* (1- *expansion-factor*) (or (position x empty-rows :test '<) 0)))
          (+ y (* (1- *expansion-factor*) (or (position y empty-columns :test '<) 0))))))

(defun distance-expanded (pos-1 pos-2 empty-rows empty-columns)
  (let ((new-1 (coordinate-expanded pos-1 empty-rows empty-columns))
        (new-2 (coordinate-expanded pos-2 empty-rows empty-columns)))
    (+ (abs (- (first new-2) (first new-1)))
       (abs (- (second new-2) (second new-1))))))

(defun collect-galaxies (grid)
  (let ((galaxies ()))
    (do-array (i j x grid galaxies)
      (when (char= x #\#)
        (push (list i j) galaxies)))))

(defun answer-ex-11-1 ()
  (let* ((grid (read-file-as-array "../inputs/input11.txt"))
         (empty-rows (empty-rows grid))
         (empty-columns (empty-columns grid))
         (galaxies (collect-galaxies grid)))
    (loop :for (galaxy-1 . rest) :on galaxies
          :sum (loop :for galaxy-2 :in rest
                     :sum (distance-expanded galaxy-1 galaxy-2 empty-rows empty-columns)))))

(defun answer-ex-11-2 ()
  (let* ((grid (read-file-as-array  "../inputs/input11.txt"))
         (empty-rows (empty-rows grid))
         (empty-columns (empty-columns grid))
         (galaxies (collect-galaxies grid))
         (*expansion-factor* 1000000))
    (loop :for (galaxy-1 . rest) :on galaxies
          :sum (loop :for galaxy-2 :in rest
                     :sum (distance-expanded galaxy-1 galaxy-2 empty-rows empty-columns)))))
