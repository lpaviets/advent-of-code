(in-package #:aoc2023/ex24)

(defun parse-line (line)
  (destructuring-bind (px py pz vx vy vz)
      (collect-integers-in-line line)
    (list (list px py pz) (list vx vy vz))))

;; Given two lines a + rv, b + su, they intersect at c if
;; c = a + r1v = b + s1u
;; so we get a system
;;
;; vx R - ux S = bx - ax  (1)
;; vy R - uy S = by - ay  (2)
;; and similar for the z direction
;;
;; This means that we have, from (2):
;;
;; R = (uy S + by - ay) / vy
;;
;; and so plugging this into (1):
;;
;; (uy S + by - ay) vx/vy - ux S = bx - ax
;;
;; and so
;;
;; (vx uy / vy - ux)S = bx - ax + (ay - by)vx/vy

;; No intersection if ux vy = vx uy.

(defun x-y-intersection (line-a line-b)
  (destructuring-bind ((ax ay az) (vx vy vz)) line-a
    (declare (ignore az vz))
    (destructuring-bind ((bx by bz) (ux uy uz)) line-b
      (unless (= (* ux vy) (* vx uy))
        (let* ((s (/ (+ bx (- ax) (* vx (- ay by) (/ vy)))
                     (- (* vx uy (/ vy)) ux)))
               (r (/ (+ by (- ay) (* uy s))
                     vy)))
          (values
           (mapcar '+
                   (list bx by bz)
                   (mapcar (lambda (v) (* s v)) (list ux uy uz)))
           s
           r))))))

(defun in-range-p (point low high)
  (destructuring-bind (ax ay az) point
    (declare (ignore az))
    (and (<= low ax high)
         (<= low ay high))))

;; Part 2: need to perform matrix inversion, and quite a bit of math.

(defun answer-ex-24-1 ()
  (let ((lines (read-file-as-lines "../inputs/input24.txt" :parse #'parse-line))
        (low 200000000000000)
        (high 400000000000000))
    (loop :for (la . rest) :on lines
          :sum (loop :for lb :in rest
                     :for (int s r) = (multiple-value-list
                                       (x-y-intersection la lb))
                     :count (and int
                                 (plusp s)
                                 (plusp r)
                                 (in-range-p int low high))))))

(defun answer-ex-24-2 ())
