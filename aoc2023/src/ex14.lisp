(in-package #:aoc2023/ex14)

(defun get-score (grid)
  (let ((height (array-dimension grid 0))
        (acc 0))
    (do-array (i j x grid acc)
      (when (char= x #\O)
        (incf acc (- height i))))))

(defun tilt-grid (grid)
  (loop :with width = (array-dimension grid 1)
        :with height = (array-dimension grid 0)
        :for j :below width
        :do (loop :with last-rock = -1
                  :for i :below height
                  :for obj = (aref grid i j)
                  :do (case obj
                        (#\# (setf last-rock i))
                        (#\O
                         (incf last-rock)
                         (setf (aref grid i j) #\.
                               (aref grid last-rock j) obj)))))
  grid)

(defun rotate-grid (grid)
  (let ((new-grid (make-array (reverse (array-dimensions grid))))
        (height (array-dimension grid 0)))
    (do-array (i j x grid new-grid)
      (setf (aref new-grid j (- height i 1)) x))))

(defun tilt-cycle (grid)
  (let ((rotated grid))
    (dotimes (i 4)
      (tilt-grid rotated)
      (setf rotated (rotate-grid rotated)))
    (loop :for i :below (array-total-size rotated)
          :do (setf (row-major-aref grid i) (row-major-aref rotated i)))
    grid))

;; 1000000000 cycles ?
;; Keep a hash-table: SCORE -> ((GRID . STEP)*)
;; When we find an already-seen score: loop over the grids that achieved it
;; If some is equal, we found the period
(defun find-period (grid)
  (loop :with score->grids = (make-hash-table :test 'eql)
        :with step->score = (make-array 1000 :adjustable t
                                             :fill-pointer 1
                                             :initial-element 0)
        :for i :from 1
        :for score = (get-score (tilt-cycle grid))
        :for new = (deepcopy grid)
        :for same-score = (gethash score score->grids)
        :for same-step = (cdr (assoc new same-score :test 'equalp))
        :do (vector-push-extend score step->score)
        :when same-step
          :do (let* ((period (- i same-step))
                     (rem (mod (- 1000000000 i) period))
                     (result (aref step->score (+ rem same-step))))
                (return-from find-period result))
        :do (push (cons new i) (gethash score score->grids))))

(defun answer-ex-14-1 ()
  (let ((grid (read-file-as-array "../inputs/input14.txt")))
    (get-score (tilt-grid grid))))

(defun answer-ex-14-2 ()
  (let ((grid (read-file-as-array "../inputs/input14.txt")))
    (find-period grid)))
