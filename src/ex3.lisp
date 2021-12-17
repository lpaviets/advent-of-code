(in-package #:aoc2021/ex3)

(defun pick-frequent-digit (list &optional default)
  (case (signum (- (count #\0 list)
                   (count #\1 list)))
    (1 #\0)
    (-1 #\1)
    (0 default)))

(defun accumulate-digits (list)
  (map 'string
       'pick-frequent-digit
       (apply 'map 'list 'list list)))

(defun gamma (list)
  (parse-integer (accumulate-digits list) :radix 2))

(defun epsilon (list)
  (let ((flip (map 'string
                    (flip #\0 #\1)
                    (accumulate-digits list))))
    (parse-integer flip :radix 2)))

;; Part 2

(defun filter-select-digit (list pos most-or-least)
  (let ((list-chars (mapcar (lambda (line) (char line pos)) list)))
    (if (eq most-or-least :most)
        (pick-frequent-digit list-chars #\1)
        (funcall (flip #\0 #\1) (pick-frequent-digit list-chars #\1)))))

(defun filter-common (list pos most-or-least)
  (loop :with select-digit = (filter-select-digit list pos most-or-least)
        :for line :in list
        :if (char= select-digit (char line pos))
          :collect line))

(defun oxygen (list)
  (do ((i 0 (1+ i))
       (filtered list (filter-common filtered i :most)))
      ((not (cdr filtered)) (parse-integer (car filtered) :radix 2))))

(defun co2 (list)
  (do ((i 0 (1+ i))
       (filtered list (filter-common filtered i :least)))
      ((not (cdr filtered)) (parse-integer (car filtered) :radix 2))))

(defun answer-ex-3-1 ()
  (let ((list (read-file-as-lines "../inputs/input3.txt")))
    (* (gamma list)
       (epsilon list))))

(defun answer-ex-3-2 ()
  (let ((list (read-file-as-lines "../inputs/input3.txt")))
    (* (oxygen list)
       (co2 list))))


;;; Alternative hackish version

(defun %sum-char (x y)
  (- (+ x (char-code y))
     (+ 0.5 (char-code #\0))))

(defun %sum-array (x y)
  (map 'vector '%sum-char x y))

(defun %sum-lines (lines)
  (reduce '%sum-array lines :initial-value (make-array 12 :initial-element 0)))

(defun %most-common (lines)
  (map 'string
       (lambda (x)
         (if (plusp x) #\0 #\1))
       (%sum-lines lines)))

(defun %gamma (lines)
  (parse-integer (%most-common lines) :radix 2))

(defun %epsilon (lines)
  (parse-integer (map 'string (flip #\0 #\1) (%most-common lines))
                 :radix 2))
