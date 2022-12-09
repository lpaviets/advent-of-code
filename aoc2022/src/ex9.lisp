(in-package #:aoc2022/ex9)

(defparameter *knots* (let ((knots (make-array 10)))
                        (dotimes (i 10)
                          (setf (aref knots i) (point 0 0)))
                        knots)
  "Knots are ordered from the HEAD (knot 0) to the end of the
rope (knot 9)")
(defparameter *head* (point 0 0))
(defparameter *tail* (point 0 0))
(defparameter *positions* (make-hash-table :test 'equal :size 200))

(declaim (inline knot (setf knot)))
(defun knot (i)
  (aref *knots* i))

(defun (setf knot) (val i)
  (setf (aref *knots* i) val))

(defun reset ()
  (dotimes (i 10)
    (setf (knot i) (point 0 0)))
  (setf *positions* (make-hash-table :test 'equal :size 200)))

(defun parse-line (line)
  (destructuring-bind (dir . num)
      (split-word-int line)
    (cons (read-from-string dir) num)))

;;;; Debug
(defun print-grid ()
  (let ((grid (make-array (list 21 21) :initial-element #\.))
        (head (knot 0)))
    (setf (aref grid 11 11) 'H)
    (dotimes (i 9)
      (with-point (x y) (sub-point head (knot (1+ i)))
        (setf (aref grid (+ 11 x) (+ 11 y)) (1+ i))))
    (print-array grid)
    nil))

;;;; Utilities
;;;; All functions are passed as argument the "current" knot
(defun diff-with-previous-knot (i)
  (sub-point (knot (1- i)) (knot i)))

(defun adjacentp (i)
  (with-point (x y) (diff-with-previous-knot i)
    (<= (max (abs x) (abs y)) 1)))

(defun catch-up (i)
  (unless (adjacentp i)
    (with-point (dx dy) (diff-with-previous-knot i)
      (case (min (abs dx) (abs dy))
        (0 (setf (knot i) (add-point (knot i)
                                     (point (truncate dx 2) (truncate dy 2)))))
        (1 (if (= (abs dx) 2) ;; furthest on x coordinate
               (setf (knot i) (add-point (knot i)
                                         (point (truncate dx 2) dy)))
               (setf (knot i) (add-point (knot i)
                                         (point dx (truncate dy 2))))))))))

(defun catch-up (i)
  (unless (adjacentp i)
    (with-point (dx dy) (diff-with-previous-knot i)
      (case (min (abs dx) (abs dy))
        (0 (setf (knot i) (add-point (knot i)
                                     (point (- dx (signum dx))
                                            (- dy (signum dy))))))
        (t
         (cond
           ((< (abs dx) (abs dy))
            (setf (knot i) (add-point (knot i)
                                      (point dx (- dy (signum dy))))))
           (t
            (setf (knot i) (add-point (knot i)
                                      (point (- dx (signum dx)) dy))))))))))


(defun update-head (dir)
  (ecase dir
    (R (incf (point-x (knot 0))))
    (L (decf (point-x (knot 0))))
    (U (incf (point-y (knot 0))))
    (D (decf (point-y (knot 0))))))

(defun update-positions-tracker (i)
  (with-point (x y) (knot i)
   (setf (gethash (cons x y) *positions*) t)))

(defun play-unit-move (dir tracking &optional debug)
  (when debug (format t "~2&Moving in dir ~S~2%" dir))
  (update-head dir)
  (dotimes (i 9)
    (catch-up (1+ i)))
  (when debug
    (print-grid)
    (terpri))
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
