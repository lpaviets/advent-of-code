(in-package #:aoc2024/ex3)

(defparameter *input* #P"input")
(defparameter *test* #P"test")

(defun parse-mults-in-line (line)
  (let ((res 0))
   (ppcre:do-register-groups ((#'parse-integer x y))
       ("mul\\((\\d+),(\\d+)\\)" line res)
     (incf res (* x y)))))

(defun split-on-dos-donts (line)
  (let* ((parts (ppcre:split "do\\(\\)" line)))
    (mapcar (lambda (part)
              (car (ppcre:split "don't\\(\\)" part :limit 2)))
            parts)))

(defun answer-ex-3-1 (file)
  (loop :for line :in (read-file-as-lines file)
        :sum (parse-mults-in-line line)))

(defun answer-ex-3-2 (file)
  (let* ((lines (read-file-as-lines file))
         (one-line (apply 'concatenate 'string lines))) ; reinvents the wheel
    (reduce '+ (mapcar 'parse-mults-in-line (split-on-dos-donts one-line)))))
