(in-package #:aoc2022/ex15)

(defparameter *sensors* nil)

(defclass sensor ()
  ((pos :initarg :pos :reader pos :type point)
   (distance :initarg :distance :reader distance)))

(defun manhattan-distance (point-a point-b)
  (with-point (x y) (sub-point point-a point-b)
    (+ (abs x) (abs y))))

(defun make-sensor (x y beacon-x beacon-y)
  (let ((pos (point x y))
        (beacon-pos (point beacon-x beacon-y)))
   (make-instance 'sensor
                  :pos pos
                  :distance (manhattan-distance pos beacon-pos))))

(defun parse-line (line)
  (apply #'make-sensor
         (mapcar #'parse-integer
                 (ppcre:all-matches-as-strings "\\d+" line))))

(defun parse-input (file)
  (setf *sensors* nil)
  (dolist (sensor (read-file-as-lines file :parse #'parse-line))
    (push sensor *sensors*))
  *sensors*)

;;;; We stop working with sensors and beacons, and realize that the problem
;;;; is to determine the surface/interval covered by (tilted) squares/intervals.
;;;; Indeed, a square is empty if it belongs to some sensor's "neighbourhood"
;;;; We are then measuring the surface covered by those
(defun bounds-covered (sensor y-line)
  "Returns a list of two integers, respectively the leftmost and rightmost
points covered by a given SENSOR on line Y-LINE"
;;; Search for x such that (distance sensor) == (manhattan (x y-line) sensor)
;;; i.e. with (sx, sy) = (pos sensor) and d = (distance sensor):
;;;     d    = |sy - y| + |sx - x|
;;; |sx - x| = d - |sy - y|
;;;  sx - x  = +- (d - |sy - y|)
;;;     x    = sx +- (d - |sy - y|)
  (let ((d (distance sensor)))
    (with-point (sx sy) (pos sensor)
      (let ((offset (- d (abs (- sy y-line)))))
        (list (- sx offset)
              (+ sx offset))))))

(defun merge-intervals (intervals)
  "Takes a list of intervals, sorted by their starting point
Returns a list of disjoint intervals covering the same points"
  (cond
    ((endp (cdr intervals)) nil)
    (t (destructuring-bind (min-a max-a) (first intervals)
         (destructuring-bind (min-b max-b) (second intervals)
           (if (<= min-b (1+ max-a))
               (merge-intervals (cons (list min-a (max max-a max-b))
                                      (cddr intervals)))
               (cons (first intervals)
                     (merge-intervals (cdr intervals)))))))))

(defun merge-interval-to-list (intervals new)
  (loop :with (minb maxb) = new
        ))

(defun answer-ex-15-1 ())

(defun answer-ex-15-2 ())
