(in-package #:aoc2021/ex22)

(declaim (optimize (debug 3) (speed 0) (safety 0)))

(defstruct cube
  (x1 0 :type integer)
  (x2 0 :type integer)
  (y1 0 :type integer)
  (y2 0 :type integer)
  (z1 0 :type integer)
  (z2 0 :type integer)
  (covered nil :type list)
  (on t :type boolean))

(defun read-cube (line)
  (let* ((elements (ppcre:split " \|," line))
         (is-on (string= "on" (car elements)))
         (bounds (mapcar (lambda (str)
                           (ppcre:register-groups-bind (('parse-integer low)
                                                        ('parse-integer high))
                               ("\\w=\([-0-9]+\)..\([-0-9]+\)" str)
                             (list low high)))
                         (cdr elements))))
    (cons is-on bounds)))

(defun count-unit-cubes (bounds)
  (apply '* (mapcar (lambda (x)
                      (1+ (- (second x) (first x))))
                    bounds)))

(defun set-all-unit-cubes (bounds val array offset)
  (destructuring-bind ((x1 x2) (y1 y2) (z1 z2))
      (loop :for off :in offset
            :for (x y) :in bounds
            :collect (list (+ x off)
                           (+ y off)))

    (loop :for x :from x1 :to x2 :do
      (loop :for y :from y1 :to y2 :do
        (loop :for z :from z1 :to z2 :do
          (setf (aref array x y z) val))))))

(defun count-on-cubes (array)
  (loop :for i :below (array-total-size array)
        :count (row-major-aref array i)))

(defmethod intersect-p ((bounds-a list) (bounds-b list))
  (destructuring-bind ((xa1 xa2) (ya1 ya2) (za1 za2)) bounds-a
    (destructuring-bind ((xb1 xb2) (yb1 yb2) (zb1 zb2)) bounds-b
      (not (or (< xa2 xb1)
               (< xb2 xa1)
               (< ya2 yb1)
               (< yb2 ya1)
               (< za2 zb1)
               (< zb2 za1))))))

(defmacro with-coords ((x1 x2 y1 y2 z1 z2) cube &body body)
  `(with-slots ((,x1 x1) (,x2 x2)
                (,y1 y1) (,y2 y2)
                (,z1 z1) (,z2 z2))
       ,cube
     ,@body))

(defmethod intersect-p ((cube-a cube) (cube-b cube))
  (with-coords (xa1 xa2 ya1 ya2 za1 za2) cube-a
    (with-coords (xb1 xb2 yb1 yb2 zb1 zb2) cube-b
      (not (or (< xa2 xb1) (< xb2 xa1)
               (< ya2 yb1) (< yb2 ya1)
               (< za2 zb1) (< zb2 za1))))))

;;; Part 2
;;; NOTE: In this paragraph, "cube" is a shorthard for any axis-parallel
;;; polyhedron. These are not literal cubes.
;;; Need to find a more efficient data structure than an array
;;; where 1 array element <-> 1 unit cube
;;; Solution: each cube is always "on", and we keep track of all subcubes
;;; that were removed from it, as the intersection of cubes is always a
;;; cube (for the aforementioned meaning of "cube")
;;; More precisely: each "cube" object keeps a list of cubes that cover it
;;; Each of those "covering" is itself full of holes.
;;; When we 'add' a cube, we essentially say to all the previous cubes that
;;; they no longer have the control of its area

(defun cube-from-bounds (bounds &optional on)
  (destructuring-bind ((x1 x2) (y1 y2) (z1 z2)) bounds
    (make-cube :x1 x1 :x2 x2
               :y1 y1 :y2 y2
               :z1 z1 :z2 z2
               :on on)))

(defun contained (cube-a cube-b)
    (with-coords (xa1 xa2 ya1 ya2 za1 za2) cube-a
      (with-coords (xb1 xb2 yb1 yb2 zb1 zb2) cube-b
        (and (<= xa1 xb1 xb2 xa2)
             (<= ya1 yb1 yb2 ya2)
             (<= za1 zb1 zb2 za2)))))

(defmethod cube-intersection ((cube-a cube) (cube-b cube))
  (when (intersect-p cube-a cube-b)
   (with-coords (xa1 xa2 ya1 ya2 za1 za2) cube-a
     (with-coords (xb1 xb2 yb1 yb2 zb1 zb2) cube-b
       (let ((xs (sort (list xa1 xa2 xb1 xb2) '<))
             (ys (sort (list ya1 ya2 yb1 yb2) '<))
             (zs (sort (list za1 za2 zb1 zb2) '<)))
         (make-cube :x1 (second xs)
                    :x2 (third xs)
                    :y1 (second ys)
                    :y2 (third ys)
                    :z1 (second zs)
                    :z2 (third zs)))))))

(defmethod add-cube ((cube-a cube) (cube-b cube))
  (let ((int-cube (cube-intersection cube-a cube-b)))
    (when int-cube
      (remove-cube cube-a int-cube))))

(defmethod remove-cube ((cube-a cube) (cube-b cube))
  (dolist (covered (cube-covered cube-a))
    (add-cube covered cube-b))
  (push cube-b (cube-covered cube-a)))

(defmethod volume ((cube cube))
  (with-coords (x1 x2 y1 y2 z1 z2) cube
    (let ((raw-vol (* (- (1+ x2) x1)
                      (- (1+ y2) y1)
                      (- (1+ z2) z1))))
      (dolist (cov (cube-covered cube))
        (decf raw-vol (volume cov)))
      raw-vol)))

(defun build-cubes (bounds-list)
  (loop :with seen-cubes
        :for (on . bounds) :in bounds-list
        :for cube = (cube-from-bounds bounds on) :do
          (dolist (other seen-cubes)
            (add-cube other cube))
        :when on
          :do (push cube seen-cubes)
        :finally (return seen-cubes)))

(defun total-volume (cubes)
  (loop :for cube :in cubes
        :sum (volume cube)))

(defun answer-ex-22-1 ()
  (let ((lines (read-file-as-lines "../inputs/input22.txt"
                                   :parse #'read-cube))
        (array (make-array '(101 101 101) :initial-element nil))
        (offset '(50 50 50))
        (default-bounds '((-50 50) (-50 50) (-50 50))))

    (loop :for (on . bounds) :in lines
          :when (intersect-p bounds default-bounds)
            :do (set-all-unit-cubes bounds on array offset))

    (count-on-cubes array)))

(defun answer-ex-22-2 ()
  (let ((lines (read-file-as-lines "../inputs/input22.txt"
                                               :parse #'read-cube)))
    (total-volume (build-cubes lines))))
