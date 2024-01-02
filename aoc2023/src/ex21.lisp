(in-package #:aoc2023/ex21)

(defparameter *visited-a* (make-hash-table :test #'equal)
  "All the positions visited at the current step.")
(defparameter *visited-b* (make-hash-table :test #'equal)
  "All the positions visited at the previous step.")
(defparameter *visited-c* (make-hash-table :test #'equal)
  "Newly visited positions, between last and current step.")

;; OLD + DIFF = NEW
;; NEW2 = OLD+N(DIFF)

(defun make-step (grid)
  (let ((new (make-hash-table :test #'equal
                              :size (hash-table-size *visited-c*))))
    (do-hashkeys (pos *visited-c*)
      (dolist (nghb (grid-neighbours pos grid :walls #\# :torus t))
        (unless (gethash nghb *visited-b*)
          (setf (gethash nghb new) t))
        (setf (gethash nghb *visited-b*) t)))
    (setf *visited-c* new))
  (rotatef *visited-a* *visited-b*))

(defun make-steps (grid n)
  (dotimes (i n)
    (make-step grid)))

(defun find-start (grid)
  (do-array (i j x grid)
    (when (char= x #\S)
      (setf (aref grid i j) #\.)
      (return-from find-start (list i j)))))

(defun explored-to-grid (grid)
  (let ((new-grid (deepcopy grid)))
    (do-hashkeys (pos *visited-a*)
      (setf (aref new-grid (first pos) (second pos)) #\O))
    new-grid))

(defun reset (file)
  (let* ((grid (read-file-as-array file))
         (start (find-start grid)))
    (setf *visited-a* (make-hash-table :test #'equal)
          *visited-b* (make-hash-table :test #'equal)
          *visited-c* (make-hash-table :test #'equal))
    (setf (gethash start *visited-a*) t
          (gethash start *visited-c*) t)
    grid))

;; Notice that:
;; - we have 2 increasing sequences when looking at actual positions
;; (STEP+2 contains the same cells than STEP, or more)
;; - after 131 steps (so, 132 for the other parity), the sequence is
;; stable. That is, when entering a square from any of its 4 sides,
;; it is "filled" after 131/132 steps.
;; The squares visited are increasing as a quadratic sequence.
;; We therefore only need to know at which step we enter each square,
;; and/or how many squares of a given type we have at each step.

(defun answer-ex-21-1 (&optional (n 64))
  (let ((grid (reset "test.txt";; "../inputs/input21.txt"
                     )))
    (dotimes (i n)
      (make-step grid))
    (hash-table-count *visited-a*)
    (grid-print (explored-to-grid grid))))

(defun answer-ex-21-2 ()
  (let* ((grid (reset "test.txt")))
    grid))
