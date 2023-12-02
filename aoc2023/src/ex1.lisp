(in-package #:aoc2023/ex1)

;; Part 1
(defun collect-digits (line)
  (cl-ppcre:all-matches-as-strings "\\d" line))

(defun merge-first-last (digits)
  (parse-integer (concatenate 'string (first digits) (car (last digits)))))

;; Part 2
(defparameter *all-numbers*
  (loop :for i :from 1 :to 9
        :collect (cons (format nil "~R" i) i)
        :collect (cons (format nil "~D" i) i)))

(defun read-digit (d)
  (cdr (assoc d *spelled-numbers* :test 'string-equal)))

;; Painful to do with regexp: overlapping word are tricky to handle.
;; For example, a line ending with "...oneight" is somewhat hard to
;; deal with.

;; So we do a braindead search over all possible "digits", from the
;; start and from the end.
(defun first-digit (line)
  (cdr (argmin *all-numbers*
               :key (lambda (elem)
                      (search (car elem) line :test 'string-equal))
               :exclude-null t)))

(defun last-digit (line)
  (cdr (argmax *all-numbers*
               :key (lambda (elem)
                      (search (car elem) line :test 'string-equal :from-end t))
               :exclude-null t)))

(defun parse-extended-line (line)
  (+ (* 10 (first-digit line)) (last-digit line)))

(defun answer-ex-1-1 ()
  (reduce '+ (read-file-as-lines "../inputs/input1.txt"
                                 :parse (lambda (line)
                                          (merge-first-last (collect-digits line))))))

(defun answer-ex-1-2 ()
  (reduce '+ (read-file-as-lines "../inputs/input1.txt"
                                 :parse 'parse-extended-line)))
