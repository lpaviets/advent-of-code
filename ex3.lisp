(in-package #:aoc2021/ex3)

(defun accumulate-digits (list)
  (let ((count-digits (make-array 12
                                  :element-type 'integer
                                  :initial-element 0)))
    (loop :for line :in list
          :do
             (loop :for i :below 12
                   :for char = (char line i)
                   :when (char= #\0 char)
                     :do (incf (aref count-digits i))))
    count-digits))

(defun get-gamma (list)
  (let* ((majority (/ (length list) 2))
         (counts (accumulate-digits list))
         (most-common (map 'string
                           (lambda (x)
                             (if (> x majority) #\1 #\0))
                           counts)))
    (parse-integer most-common :radix 2)))

(defun get-epsilon (list)
  (let* ((majority (/ (length list) 2))
         (counts (accumulate-digits list))
         (most-common (map 'string
                           (lambda (x)
                             (if (> x majority) #\0 #\1))
                           counts)))
    (parse-integer most-common :radix 2)))

;; Part 2

(defun filter-common (list pos most-or-least)
  (let ((select-digit (loop :for line :in list
                            :for char = (char line pos)
                            :if (char= #\0 char)
                              :count 1 :into zeros
                            :else
                              :count 1 :into ones
                            :finally (return (if (eq most-or-least :most)
                                                 (if (>= ones zeros) #\1 #\0)
                                                 (if (> zeros ones) #\1 #\0))))))
    (loop :for line :in list
          :for char = (char line pos)
          :if (char= select-digit char)
            :collect line)))

(defun get-oxygen (list)
  (do ((i 0 (1+ i))
       (filtered list (filter-common filtered i :most)))
      ((not (cdr filtered)) (parse-integer (car filtered) :radix 2))))

(defun get-co2 (list)
  (do ((i 0 (1+ i))
       (filtered list (filter-common filtered i :least)))
      ((not (cdr filtered)) (parse-integer (car filtered) :radix 2))))

(defun answer-ex-3-1 ()
  (let ((list (read-file-as-lines "inputs/input3.txt")))
    (* (get-gamma list)
       (get-epsilon list))))

(defun answer-ex-3-2 ()
  (let ((list (read-file-as-lines "inputs/input3.txt")))
    (* (get-oxygen list)
       (get-co2 list))))
