(in-package #:aoc2023/ex13)

(defun symmetric-row-p (row grid)
  "ROW is a half-integer, i.e. some integer +1/2

The function returns T if the rows of GRID are symmetric relative to
the horizontal line of height ROW"
  (loop :with height = (array-dimension grid 0)
        :with width = (array-dimension grid 1)
        :with max-reflection = (min (ceiling row)
                                    (floor (- height row)))
        :repeat max-reflection
        :for i :from 1/2
        :always (loop :for j :from 0 :below width
                      :always (eql (aref grid (+ row i) j)
                                   (aref grid (- row i) j)))))

(defun symmetric-column-p (column grid)
  "COLUMN is a half-integer, i.e. some integer +1/2

The function returns T if the columns of GRID are symmetric relative to
the horizontal line of height COLUMN"
  (loop :with height = (array-dimension grid 0)
        :with width = (array-dimension grid 1)
        :with max-reflection = (min (ceiling column)
                                    (floor (- width column)))
        :repeat max-reflection
        :for j :from 1/2
        :always (loop :for i :from 0 :below height
                      :always (eql (aref grid i (+ column j))
                                   (aref grid i (- column j))))))

(defun find-row-symmetry (grid &key except)
  (loop :with height = (array-dimension grid 0)
        :for row :from 1/2 :below (1- height)
        :when (and (or (not except)
                       (/= except row))
                   (symmetric-row-p row grid))
          :do (return row)))

(defun find-column-symmetry (grid &key except)
  (loop :with width = (array-dimension grid 1)
        :for column :from 1/2 :below (1- width)
        :when (and (or (not except)
                       (/= except column))
                   (symmetric-column-p column grid))
          :do (return column)))

;; Problem: can find the same symmetry again
(defun find-new-symmetry-score (grid)
  (let ((row (find-row-symmetry grid))
        (col (find-column-symmetry grid)))
    (do-array (i j x grid)
      (setf (aref grid i j) (if (char= x #\.) #\# #\.))
      (let ((new-row (find-row-symmetry grid :except row))
            (new-col (find-column-symmetry grid :except col)))
        (when new-row
          (return-from find-new-symmetry-score (* 100 (ceiling new-row))))
        (when new-col
          (return-from find-new-symmetry-score (ceiling new-col)))
        (setf (aref grid i j) x)))))

(defun answer-ex-13-1 ()
  (loop :for grid :in (read-file-as-lines-blocks "../inputs/input13.txt"
                                                 :parse-block 'read-array)
        :for row-sym = (find-row-symmetry grid)
        :if row-sym
          :sum (* 100 (ceiling row-sym))
        :else
          :sum (ceiling (find-column-symmetry grid))))

(defun answer-ex-13-2 ()
  (loop :for grid :in (read-file-as-lines-blocks "../inputs/input13.txt"
                                                 :parse-block 'read-array)
        :sum (find-new-symmetry-score grid)))
