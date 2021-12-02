;;;; 2021.lisp

(in-package #:aoc2021)

(defun read-file-as-lines (filename &key (parse 'identity))
  "Read file into a list of lines."
  (with-open-file (in filename)
    (loop :for line = (read-line in nil nil)
          :while line
          :collect (funcall parse line))))


(defun read-file-as-integers (filename)
  (read-file-as-lines filename :parse 'parse-integer))

(defun split-word-int (line)
  (ppcre:register-groups-bind (word (#'parse-integer int))
      ("\(\\w+\) \(\\d+\)" line)
    (cons word int)))
