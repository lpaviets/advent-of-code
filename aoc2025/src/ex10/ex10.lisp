(in-package #:aoc2025/ex10)

(defparameter *input* #P"input")
(defparameter *test* #P"test")

(defun as-sexp (str)
  (loop :for (x pos) = (multiple-value-list (read-from-string str nil :end))
          :then  (multiple-value-list (read-from-string str nil :end :start pos))
        :while (not (eq x :end))
        :collect x))

(defun parse-lights (lights)
  (map 'bit-vector (lambda (c)
                     (if (char= c #\#) 1 0))
       lights))

(defun parse-buttons (buttons n)
  (coerce (loop :for button :in buttons
                :for array = (make-array n :element-type 'bit :initial-element 0)
                :do (dolist (x button)
                      (setf (aref array x) 1))
                :collect array)
          'vector))

(defun parse-line (line)
  (destructuring-bind (lights buttons rest)
      (cdr (cl-ppcre:split "[]{}[]" (substitute #\Space #\, line)))
    (list (parse-lights lights)
          (parse-buttons (as-sexp buttons) (length lights))
          (as-sexp rest))))

(defun read-instructions (file)
  (read-file-as-lines file :parse 'parse-line))

(defun matches-p (lights selected)
  (let ((n (length lights)))
    (loop :for i :below n
          :for light-on-p = (aref lights i)
          :for selected-on-p = (loop :for button :across selected
                                     :sum (aref button i))
          :always (= light-on-p (mod selected-on-p 2)))))

(defun match-lights-pattern (lights buttons)
  (let ((n (length lights)))
    (dotimes (k (1- n))
      (do-sequence-subsets (pushed ((1+ k) buttons))
        (when (matches-p lights pushed)
          (return-from match-lights-pattern (1+ k)))))))

(defun answer-ex-10-1 (file)
  (loop :for (lights buttons) :in (read-instructions file)
        :sum (match-lights-pattern lights buttons)))

(defun answer-ex-10-2 (file))
