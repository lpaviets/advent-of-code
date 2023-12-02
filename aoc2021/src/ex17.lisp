(declaim (optimize (speed 3) (safety 0) (debug 0)))

(in-package #:aoc2021/ex17)

(defun parse-target (string)
  (ppcre:register-groups-bind (('parse-integer xmin)
                               ('parse-integer xmax)
                               ('parse-integer ymin)
                               ('parse-integer ymax))
      ("target area: x=\([-0-9]+\)..\([-0-9]+\), y=\([-0-9]+\)..\([-0-9]+\)" string)
    (list xmin xmax ymin ymax)))

(defun sum-n (n)
  (declare (ftype (function (fixnum) fixnum) sum-n))
  (truncate (* n (+ 1 n))
            2))

;;; Part 1, explanation (Disregard the horizontal velocity for now)
;;; If we start with a positive vertical velocity of y, by the moment
;;; we come back to the position 0, the velocity will be -y.
;;;
;;; Thus, the probe *leaves* this position with a speed of (-y + 1).
;;; The maximal height that the probe ever attains is an increasing
;;; function of y, so we need to find the largest y so that the
;;; sequence hits the target area. If the target area has a cell with
;;; a negative vertical coordinate -n, then y = n-1 works

;;; Now, for x. We can take any x that places us above the target area
;;; in 2y steps. If we start with a velocity of x, the rightmost coordinate
;;; that we attain is x(x+1)/2, after x steps

(defun velocity-max-height (target-area)
  (declare (ftype (function (list) list) velocity-max-height))
  (destructuring-bind (xmin xmax ymin ymax) target-area
    (declare (type fixnum xmin xmax ymin ymax))
    (let* ((x-candidate (isqrt (* 2 xmin)))
           (x (loop :for x :from x-candidate
                    :thereis (and (<= xmin
                                      (sum-n x)
                                      xmax)
                                  x))))
      (list x (1- (abs (min ymin ymax)))))))

(defun max-height (velocity)
  (declare (ftype (function (list) fixnum) max-height))
  (sum-n (second velocity)))

;;; Part 2
(defun in-target-area (pos target)
  (destructuring-bind (x1 x2 y1 y2) target
    (declare (type fixnum x1 x2 y1 y2)
             (type (cons fixnum (cons fixnum null)) pos))
    (and (<= x1 (first pos) x2)
         (<= y1 (second pos) y2))))

(defun next-pos (pos step init-velocity)
  (declare (ftype (function (list fixnum list) list) next-pos))
  (destructuring-bind (x y) pos
    (destructuring-bind (vx vy) init-velocity
      (declare (type fixnum x y vx vy))
      (list (+ x (max (- vx step) 0))
            (+ y (- vy step))))))

(defun over-target (pos target)
  (destructuring-bind (x1 x2 y1 y2) target
    (declare (type fixnum x1 x2 y1 y2)
             (type (cons fixnum (cons fixnum null)) pos)
             (ignorable x1 y2))
    (or (> (first pos) x2)
        (< (second pos) y1))))

(defun count-velocities (target)
  (destructuring-bind (x1 x2 y1 y2) target
    (declare (ignorable x1 y2)
             (type fixnum x1 x2 y1 y2))
    (loop :for x :from (truncate (isqrt x1) 2) :to x2
          :sum
          (loop :for y :from y1 :to (- y1)
                :count
                (loop :for steps :from 0
                      :for pos = (list x y)
                        :then (next-pos pos steps (list x y))
                      :for goal = (in-target-area pos target)
                      :until (or goal (over-target pos target))
                      :finally (return goal))))))

(defun answer-ex-17-1 ()
  (let ((target (read-file-one-line "../inputs/input17.txt"
                                    :parse 'parse-target)))
    (max-height (velocity-max-height target))))

(defun answer-ex-17-2 ()
  (let ((target (read-file-one-line "../inputs/input17.txt"
                                    :parse 'parse-target)))
    (count-velocities target)))
