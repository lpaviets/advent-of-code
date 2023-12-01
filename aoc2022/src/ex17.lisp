(in-package #:aoc2022/ex17)

(defparameter *shapes* '(#2A((1 1 1 1))
                         #2A((0 1 0)
                             (1 1 1)
                             (0 1 0))
                         #2A((1 1 1)
                             (0 0 1)
                             (0 0 1))   ; note the 'top-down' mirror
                         #2A((1)
                             (1)
                             (1)
                             (1))
                         #2A((1 1)
                             (1 1))))

(defconstant +width+ 7)
(defparameter *top* (make-array +width+ :initial-element 0))

(defun shape-to-fun (shape)
  (lambda (x y)
    (let (pos)
      (do-array (i j val shape)
        (when (= val 1)
          (push (cons (+ j x) (+ i y)) pos)))
      pos)))

(defparameter *shapes-fun* nil)
(defparameter *instructions* nil)
(defparameter *step* 0)

(defun reset (&optional test)
  (setf *shapes-fun* (map 'vector #'shape-to-fun *shapes*))
  (setf *instructions* (if test ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"
                           (read-file-one-line "../inputs/input17")))
  (setf *top* (make-array +width+ :initial-element 0))
  (setf *step* 0))

(defun shape (i x y)
  (funcall (aref *shapes-fun* i) x y))

(defun spawn-pos ()
  (cons 2 (+ 4 (reduce #'max *top*))))

;; DOesn't work: can HORIZONTALLY collide with the already placed blocks (see 3rd shape of the example)
(defun in-wall-p (shape x y)
  (find nil (shape shape x y)
        :key (lambda (pos)
               (<= 0 (car pos) (1- +width+)))))

(defun in-blocks-p (shape x y)
  (loop :for (i . j) :in (shape shape x y)
        :thereis (<= j (aref *top* i))))

(defun adjust-top (shape x y)
  (loop :for (i . j) :in (shape shape x y)
        :do (setf (aref *top* i) (max j (aref *top* i)))))

(defun place-shape (shape)
  (loop :with (x . y) = (spawn-pos)
        :with xd
        :do (format t "~%Moving in dir ~C~%" (aref *instructions* *step*))
            (setf xd (if (char= (aref *instructions* *step*) #\>)
                         (1+ x)
                         (1- x)))
            (incf *step*)
        :unless (in-wall-p shape xd y)
          :do (setf x xd
                    xd (if (char= (aref *instructions* *step*) #\>)
                           (1+ x)
                           (1- x)))
              (format t "After wind: ~A, ~A~%" x y)
        :if (in-blocks-p shape x (1- y))
          :do (adjust-top shape x y)
              (format t "Stopped moving: ~A, ~A~%" x y)
              (return)
        :else
          :do (setf y (1- y))
              (format t "After fall: ~A, ~A~%" x y)
        ))

(defun test (n)
  (reset t)
  (dotimes (i n)
    (place-shape (mod i (length *shapes*))))
  *top*)

(defun answer-ex-17-1 ())

(defun answer-ex-17-2 ())
