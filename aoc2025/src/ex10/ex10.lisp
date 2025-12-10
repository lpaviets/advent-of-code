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

;; (defun match-lights-pattern (lights buttons)
;;   (let* ((n (length lights))
;;          (try (make-array n :element-type 'bit :initial-element 0)))
;;     (dotimes (k n)
;;       (do-sequence-subsets (pushed (k buttons) 'list)
;;         (dolist (button pushed)
;;           (dotimes (i n)
;;             ()))))))

(defun answer-ex-10-1 (file))

(defun answer-ex-10-2 (file))
