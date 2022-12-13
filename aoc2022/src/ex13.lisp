(in-package #:aoc2022/ex13)

(defun parse-line (line)
  (read-from-string (substitute-assoc '((#\[ . #\()
                                        (#\] . #\))
                                        (#\, . #\Space))
                                      line)))

(defun parse-input (file)
  (read-file-as-lines-blocks file :parse #'parse-line))

;;;; Comparisons do not return booleans
;;;; Instead:
;;;; return -1 if list1 < list2
;;;; return 0 if list1 = list2
;;;; return 1 if list1 > list2
(defun compare-lists (list1 list2)
  (cond
    ((null list1) (if (null list2) 0 -1))
    ((null list2) 1)
    (t (let ((first (compare (car list1) (car list2))))
         (ecase first
           ((-1 1) first) ; There was a decisive comparison
           (0 (compare (cdr list1) (cdr list2))))))))

(defun compare (obj1 obj2)
  (cond
    ((and (numberp obj1) (numberp obj2))
     (signum (- obj1 obj2)))
    ((and (listp obj1) (listp obj2))
     (compare-lists obj1 obj2))
    (t (compare (ensure-list obj1) (ensure-list obj2)))))

(defun sum-correct-indices (pairs)
  (loop :for (obj1 obj2) :in pairs
        :for i :from 1
        :when (= -1 (compare obj1 obj2))
          :sum i))

(defun merge-packets (pairs)
  (let ((packets (list '((2))
                       '((6)))))
    (loop :for (l1 l2) :in pairs
          :do (push l1 packets)
              (push l2 packets))
    packets))

(defun sort-packets (packets)
  (flet ((comp-sort (a b)
           (>= 0 (compare a b))))
    (sort packets #'comp-sort)))

(defun score-sorted (packets)
  (* (1+ (position '((2)) packets :test #'equalp))
     (1+ (position '((6)) packets :test #'equalp))))

(defun answer-ex-13-1 ()
  (sum-correct-indices (parse-input "../inputs/input13")))

(defun answer-ex-13-2 ()
  (let ((pairs (parse-input "../inputs/input13")))
    (score-sorted (sort-packets (merge-packets pairs)))))
