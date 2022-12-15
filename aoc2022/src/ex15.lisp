(in-package #:aoc2022/ex15)

;;;; This is not a good day !
;;;; All the interval arithmetic is slow and not that clear
;;;; Moreover, the second part is ugly: a better solution would
;;;; have been to use quad-trees or an advanced data structure,
;;;; but I only use basic interval arithmetic and loop for
;;;; all the possible "heights", instead of working with "boxes"
;;;; directly.

(defparameter *sensors* nil)
(defparameter *beacons* nil)

(defclass sensor ()
  ((pos :initarg :pos :reader pos :type point)
   (distance :initarg :distance :reader distance)))

(defun manhattan-distance (point-a point-b)
  (with-point (x y) (sub-point point-a point-b)
    (+ (abs x) (abs y))))

(defun point= (pt-a pt-b)
  (and (= (point-x pt-a) (point-x pt-b))
       (= (point-y pt-a) (point-y pt-b))))

(defun make-sensor (x y beacon-x beacon-y)
  (let ((pos (point x y))
        (beacon-pos (point beacon-x beacon-y)))
    (make-instance 'sensor
                   :pos pos
                   :distance (manhattan-distance pos beacon-pos))))

(defun parse-line (line)
  (let ((coords (mapcar #'parse-integer
                        (ppcre:all-matches-as-strings "-?\\d+" line))))
    (cons (apply #'make-sensor coords) ;sensor
          (point (third coords) (fourth coords))))) ; beacon

(defun parse-input (file)
  (setf *sensors* nil)
  (setf *beacons* nil)
  (dolist (sensor-beacon (read-file-as-lines file :parse #'parse-line))
    (push (car sensor-beacon) *sensors*)
    (push (cdr sensor-beacon) *beacons*))
  (setf *beacons* (remove-duplicates *beacons*
                                     :test #'point=))
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
        (unless (minusp offset)
          (list (- sx offset)
                (+ sx offset)))))))

(defun clean-and-sort-by-start (intervals)
  (sort (remove nil intervals) #'< :key #'car))

(defun merge-intervals (intervals)
  "Takes a list of intervals, sorted by their starting point
Returns a list of disjoint intervals covering the same points"
  (cond
    ((endp (cdr intervals)) intervals)
    (t (destructuring-bind (min-a max-a) (first intervals)
         (destructuring-bind (min-b max-b) (second intervals)
           (if (<= min-b (1+ max-a))
               (merge-intervals (cons (list min-a (max max-a max-b))
                                      (cddr intervals)))
               (cons (first intervals)
                     (merge-intervals (cdr intervals)))))))))

(defun point-in-intervals-p (point intervals)
  (some (lambda (interval)
          (<= (first interval) point (second interval)))
        intervals))

(defun complement-in-bounds (min max intervals)
  ;; Remove completely out-of-bounds intervals
  (setf intervals (loop :for interval :in intervals
                        :for (low high) = interval
                        :when (and (<= low max) (<= min high))
                          :collect interval))
  ;; Base case of empty list
  (when (endp intervals)
    (return-from complement-in-bounds (list min max)))
  ;; Simplify the analysis for first/last interval
  (when (< min (first (first intervals))) ; lower bound not covered by
                                        ; INTERVALS
    (push (list (1- min) (1- min)) intervals))
  (let ((last (last intervals)))
    (when (> max (second (car last)))   ; same for upper bound
      (setf (cdr last) (list (list (1+ max) (1+ max))))))
  ;; Find the complement
  (loop :for ((mina maxa) (minb maxb)) :on intervals :by #'cddr
        :while minb
        :collect (list (1+ maxa) (1- maxb))))

(defun count-covered (y)
  (let* ((sorted (clean-and-sort-by-start (mapcar (lambda (s)
                                                    (bounds-covered s y))
                                                  *sensors*)))
         (merged (merge-intervals sorted))
         (covered (loop :for (low high) :in merged
                        :sum (1+ (- high low))))
         (covered-beacons (loop :for beacon :in *beacons*
                                :for x = (point-x beacon)
                                :count (and (= y (point-y beacon))
                                            (point-in-intervals-p x merged)))))
    (- covered covered-beacons)))

(defun find-uncovered (min-x max-x y)
  (let* ((sorted (clean-and-sort-by-start (mapcar (lambda (s)
                                                    (bounds-covered s y))
                                                  *sensors*)))
         (merged (merge-intervals sorted))
         (uncovered (complement-in-bounds min-x max-x merged)))
    (when uncovered
      ;; At most one, according to the exercise
      (car (car uncovered)))))

(defun tuning-frequency (min max)
  (loop :for y :from min :to max
        :for uncovered = (find-uncovered min max y)
        :thereis (and uncovered (+ (* 4000000 uncovered) y))))

(defun answer-ex-15-1 ()
  (parse-input "../inputs/input15")
  (count-covered 2000000))

(defun answer-ex-15-2 ()
  (parse-input "../inputs/input15")
  (tuning-frequency 0 4000000))
