(in-package #:aoc2022/ex5)

(defun transpose (list-of-lines)
  (apply 'map 'list 'list list-of-lines))

(defparameter *stacks* nil)

;;; Stacks -> a vector of lists
(defun parse-input-stacks (input)
  (setf *stacks*
        (coerce
         (loop with stacks = (transpose (butlast input)) ;; last row is useless
               :for line :in (cdr stacks) :by #'cddddr ;; useless [, ], and whitespace columns
               :collect (loop :for (char . rest) :on line ;; only keep items
                              :while (char= char #\Space)
                              :finally (return (cons char rest))))
         'vector)))

(defun parse-moves (input)
  (loop :for line :in input
        :for clean-line = (substitute-if-not #\Space #'digit-char-p line)
        :collect (read-from-string (format nil "(~A)" clean-line))))

(defun move (n from to)
  (dotimes (i n)
    (push (pop (aref *stacks* (1- from))) (aref *stacks* (1- to)))))

(defun move-multiple (n from to)
  (let ((tempo ()))
    (dotimes (i n)
      (push (pop (aref *stacks* (1- from))) tempo))
    (dotimes (i n)
      (push (pop tempo) (aref *stacks* (1- to))))))

(defun run-all-moves (moves move-function)
  (loop :for (n from to) :in moves
        :do (funcall move-function n from to)))

(defun top-stacks ()
  (map 'string 'car *stacks*))

(defun answer-ex-5-1 ()
  (let* ((input (read-file-as-lines-blocks "../inputs/input5"))
         (moves (parse-moves (second input))))
    (parse-input-stacks (first input))
    (run-all-moves moves 'move)
    (top-stacks)))

(defun answer-ex-5-2 ()
  (let* ((input (read-file-as-lines-blocks "../inputs/input5"))
         (moves (parse-moves (second input))))
    (parse-input-stacks (first input))
    (run-all-moves moves 'move-multiple)
    (top-stacks)))
