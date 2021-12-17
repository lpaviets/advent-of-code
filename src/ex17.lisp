(in-package #:aoc2021/ex17)

(defun parse-target (string)
  (ppcre:register-groups-bind (('parse-integer xmin)
                               ('parse-integer xmax)
                               ('parse-integer ymin)
                               ('parse-integer ymax))
      ("target area: x=\([-0-9]+\)..\([-0-9]+\), y=\([-0-9]+\)..\([-0-9]+\)" string)
    (list xmin xmax ymin ymax)))

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

(defun answer-ex-17-1 ()
  (destructuring-bind (xmin xmax ymin ymax)
      (car (read-file-as-lines "../inputs/input17.txt"
                               :parse 'parse-target))
    (let* ((smallest-x (isqrt (* 2 xmin)))
           (x (loop :for pot-x :from smallest-x
                    :thereis (and (<= (* 2 xmin)
                                      (* pot-x (1+ pot-x))
                                      (* 2 xmax))
                                  pot-x))))
      (list x (1+ ymax)))))

(defun answer-ex-17-2 ())
