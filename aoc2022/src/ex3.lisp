(in-package #:aoc2022/ex3)

;;;; NOTE: not the best code, I do everywhere as if the intersection of
;;;; two compartments (part 1) or of the three bags in a group (part 2)
;;;; could be more than a single item
;;;; It is not a problem efficiency-wise, but it means more code and
;;;; less clarity

(defun compartments-from-bag (bag)
  (let ((size (/ (length bag) 2)))
    (list (ht-from-sequence bag :end size)
          (ht-from-sequence bag :start size))))

(defun add-item-to-compartment (item cp)
  (setf (gethash item cp) t))

(defun item-in-compartment-p (item cp)
  (gethash item cp))

(defun compartments-intersection (cp1 cp2)
  (let ((intersection (make-hash-table)))
    (do-hashkeys (x cp1)
      (if (item-in-compartment-p x cp2)
          (add-item-to-compartment x intersection)))
    (do-hashkeys (x cp2)
      (if (item-in-compartment-p x cp1)
          (add-item-to-compartment x intersection)))
    intersection))

;;; WARNING: this is NOT portable, there is NO REASON for a character
;;; to a have a char-code corresponding to what is required by AoC
;;; In particular, we might have A < a < b < B < C ... with some
;;; interleaving between uppercase & lowercase characters
(defun item-priority (item)
  (cond
    ((lower-case-p item)
     (1+ (- (char-code item) (char-code #\a))))
    ((upper-case-p item)
     (+ 27 (- (char-code item) (char-code #\A))))
    (t (error "~S is not an item, i.e. a alphabetic character~%" item))))

(defun bag-priority (bag)
  (destructuring-bind (cp1 cp2)
      (compartments-from-bag bag)
    (let ((total 0))
      (do-hashkeys (x (compartments-intersection cp1 cp2))
        (incf total (item-priority x)))
      total)))

;;; Part 2
(defun bags-split-in-group (bags)
  (loop :for (a b c) :on bags :by #'cdddr
        :collect (list
                  (ht-from-sequence a)
                  (ht-from-sequence b)
                  (ht-from-sequence c))))

(defun group-intersection (group)
  (reduce #'compartments-intersection group))

(defun group-priority (group)
  (let ((total 0))
    (do-hashkeys (x (group-intersection group))
      (incf total (item-priority x)))
    total))

(defun answer-ex-3-1 ()
  (let ((bags (read-file-as-lines "../inputs/input3")))
    (reduce #'+ bags :key #'bag-priority)))

(defun answer-ex-3-2 ()
  (let* ((bags (read-file-as-lines "../inputs/input3"))
         (groups (bags-split-in-group bags)))
    (reduce #'+ groups :key #'group-priority)))


;;;; Other solution with lists and standard INTERSECTION function
(defun %str-to-bag (str)
  (coerce str 'list))

(defun %str-to-cpts (str)
  (let ((size (/ (length str) 2)))
    (list (%str-to-bag (subseq str 0 size))
          (%str-to-bag (subseq str size)))))

(defun %item-priority (item)
  (1+ (position item "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" :test #'char=)))

(defun %cpts-prio (cpts)
  (%item-priority (car (reduce #'intersection cpts))))

(defun %split-in-groups (bags)
  (loop :for (a b c) :on bags :by #'cdddr
        :collect (mapcar '%str-to-bag (list a b c))))

(defun %answer-ex-3-1 ()
  (let ((bags (read-file-as-lines "../inputs/input3" :parse '%str-to-cpts)))
    (reduce #'+ bags :key #'%cpts-prio)))

(defun %answer-ex-3-2 ()
  (let ((bags (read-file-as-lines "../inputs/input3")))
    (reduce #'+ (%split-in-groups bags) :key #'%cpts-prio)))
