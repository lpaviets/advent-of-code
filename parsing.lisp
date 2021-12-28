;;; Advent of code:
;;; Parsing

(in-package #:advent-of-code)

;; Reading from files
(defun read-file-as-lines (filename &key parse)
  "Read file into a list of lines."
  (with-open-file (in filename)
    (loop :for line = (read-line in nil nil)
          :while line
          :collect (if parse
                       (funcall parse line)
                       line))))

(defun read-file-one-line (filename &key parse)
  "Read the first line of FILENAME, applying PARSE to it if non-NIL"
  (with-open-file (in filename)
    (let ((line (read-line in nil nil)))
      (if parse
          (funcall parse line)
          line))))

(defun read-file-as-integers (filename)
  (read-file-as-lines filename :parse 'parse-integer))

(defun read-file-as-sexprs (filename)
  "Read file as a list of s-expressions.
Each line <line> is read a the s-expr (<line>)"
  (with-open-file (in filename)
	(loop :for line = (read-line in nil nil)
		  :while line
          :collect
          (read-from-string (format nil "(~a)" line)))))

(defun read-array (list &optional (digits t))
  "Read a 2D-array. If DIGITS is non-nil, parses elements as digits"
  (loop :with array = (make-array (list (list-length list)
                                        (length (car list))))
        :for line :in list
        :for i :from 0 :do
          (loop :for c :across line
                :for j :from 0
                :for val = (if digits
                               (- (char-int c) (char-int #\0))
                               c)
                :do
                   (setf (aref array i j) val))
        :finally (return array)))

;; Other parsing utilities

(defun split-word-int (line)
  (ppcre:register-groups-bind (word (#'parse-integer int))
      ("\(\\w+\) \(\\d+\)" line)
    (cons word int)))

(defun coma-separated-int-line (line)
  (mapcar 'parse-integer (ppcre:split " *, *" line)))

(defun parse-digit (char)
  (and (char<= #\0 char #\9)
       (- (char-int char) (char-int #\0))))
