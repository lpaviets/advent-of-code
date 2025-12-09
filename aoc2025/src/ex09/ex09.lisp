(in-package #:aoc2025/ex9)

(defparameter *input* #P"input")
(defparameter *test* #P"test")

(defun read-tiles (file)
  (read-file-as-lines file :parse 'coma-separated-int-line))

(defun area-rectangle (corner1 corner2)
  (reduce '* (mapcar (lambda (x y)
                       (1+ (abs (- y x))))
                     corner1 corner2)))

(defun find-max-rectangle (tiles)
  (loop :for (c1 . rest) :on tiles
        :maximize (loop :for c2 :in rest
                        :maximize (area-rectangle c1 c2))))

(defun within-rectangle-p (c1 c2 tile)
  (destructuring-bind (x1 y1) c1
    (destructuring-bind (x2 y2) c2
      (destructuring-bind (xt yt) tile
        (when (< x2 x1)
          (rotatef x2 x1))
        (when (< y2 y1)
          (rotatef y2 y1))
        (and (<= x1 xt x2)
             (<= y1 yt y2))))))

(defun valid-rect-p (c1 c2 tiles)
  (loop :for tile :in tiles
        :always (or (equal tile c1)
                    (equal tile c2)
                    (not (within-rectangle-p c1 c2 tile)))))


(defun answer-ex-9-1 (file)
  (find-max-rectangle (read-tiles file)))

(defun answer-ex-9-2 (file))
