(in-package #:aoc2022/ex9)

(defparameter *rope-length* 10)
(defparameter *knots* (let ((knots (make-array *rope-length*)))
                        (dotimes (i *rope-length*)
                          (setf (aref knots i) (point 0 0)))
                        knots)
  "Knots are ordered starting from the HEAD (knot 0)")
(defparameter *positions* (make-hash-table :test 'equal :size 200))

(declaim (inline knot (setf knot)))
(defun knot (i)
  (aref *knots* i))

(defun (setf knot) (val i)
  (setf (aref *knots* i) val))

(defun reset ()
  (dotimes (i *rope-length*)
    (setf (knot i) (point 0 0)))
  (setf *positions* (make-hash-table :test 'equal :size 200)))

(defun parse-line (line)
  (destructuring-bind (dir . num)
      (split-word-int line)
    (cons (read-from-string dir) num)))

;;;; Utilities
;;;; All functions are passed as argument the "current" knot
(defun diff-with-previous-knot (i)
  (sub-point (knot (1- i)) (knot i)))

(defun adjacentp (i)
  (with-point (x y) (diff-with-previous-knot i)
    (<= (max (abs x) (abs y)) 1)))

(declaim (inline one-before))
(defun one-before (n)
  (- n (signum n)))

(defun update-knot-by (i dx dy)
  (let ((knot (knot i)))
    (incf (point-x knot) dx)
    (incf (point-y knot) dy)))

(defun update-head (dir)
  (ecase dir
    (R (incf (point-x (knot 0))))
    (L (decf (point-x (knot 0))))
    (U (incf (point-y (knot 0))))
    (D (decf (point-y (knot 0))))))

(defun catch-up (i)
  (unless (adjacentp i)
    (with-point (dx dy) (diff-with-previous-knot i)
      (let ((adx (abs dx))
            (ady (abs dy)))
        (cond
          ((or (= adx ady)
               (= (min adx ady) 0))
           (update-knot-by i (one-before dx) (one-before dy)))
          ((< adx ady)
           (update-knot-by i dx (one-before dy)))
          (t                            ; (> adx ady)
           (update-knot-by i (one-before dx) dy)))))))

(defun update-positions-tracker (i)
  (with-point (x y) (knot i)
   (setf (gethash (cons x y) *positions*) t)))

(defun play-unit-move (dir tracking)
  (update-head dir)
  (dotimes (i (1- *rope-length*))
    (catch-up (1+ i)))
  (update-positions-tracker tracking))

(defun play-move (dir num tracking)
  (dotimes (_ num)
    (play-unit-move dir tracking)))

(defun answer-ex-9-1 ()
  (reset)
  (let ((moves (read-file-as-lines "../inputs/input9" :parse 'parse-line)))
    (loop :for (dir . num) :in moves
          :do (play-move dir num 1))
    (hash-table-count *positions*)))

(defun answer-ex-9-2 ()
  (reset)
  (let ((moves (read-file-as-lines "../inputs/input9" :parse 'parse-line)))
    (loop :for (dir . num) :in moves
          :do (play-move dir num 9))
    (hash-table-count *positions*)))
