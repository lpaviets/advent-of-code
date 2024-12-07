(in-package #:aoc2024/ex7)

(defparameter *input* #P"input")
(defparameter *test* #P"test")

(defun parse-equations (file)
  (read-file-as-lines file :parse 'collect-integers-in-line))

(defun solve-equation (target current operands)
  (declare (optimize speed)
           (type fixnum target current))
  (cond
    ((< target current) nil)
    ((null operands) (= target current))
    (t
     (destructuring-bind (x &rest rest) operands
       (declare (type fixnum x))
       (or (solve-equation target (* current x) rest)
           (solve-equation target (+ current x) rest))))))

(defparameter *power-of-10* (coerce (loop :repeat 19
                                          :for x = 1 :then (* 10 x)
                                          :collect x)
                                    '(simple-array fixnum (19))))

(defun smallest-10^i-above (y)
  (declare (optimize speed)
           (type fixnum y))
  (loop :for i :from 0 :below 20
        :for 10^i fixnum :across (the (simple-array fixnum (19)) *power-of-10*)
        :until (< y 10^i)
        :finally (return 10^i)))

(declaim (inline ||))
(defun || (x y)
  (declare (optimize speed)
           (type fixnum x y))
  (the fixnum (+ (the fixnum (* (the fixnum (smallest-10^i-above y)) x)) y)))

(defun solve-equation-pipes (target current operands)
  (declare (optimize speed)
           (type fixnum target current))
  (cond
    ((< target current) nil)
    ((null operands) (= target current))
    (t
     (destructuring-bind (x &rest rest) operands
       (declare (type fixnum x))
       (or (solve-equation-pipes target (* current x) rest)
           (solve-equation-pipes target (+ current x) rest)
           (solve-equation-pipes target (|| current x) rest))))))

(defun answer-ex-7-1 (file)
  (loop :for (target op1 . operands) :in (parse-equations file)
        :when (solve-equation target op1 operands)
          :sum target))

(defun answer-ex-7-2 (file)
  (loop :for (target op1 . operands) :in (parse-equations file)
        :when (solve-equation-pipes target op1 operands)
          :sum target))
