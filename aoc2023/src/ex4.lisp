(in-package #:aoc2023/ex4)

(defclass card ()
  ((num :initarg :num :accessor card-num)
   (winning :initarg :winning :accessor winning)
   (picked :initarg :picked :accessor picked)))

(defun read-card (card)
  (destructuring-bind (card-num left right)
      (ppcre:split "[:|]" card)
    (make-instance 'card
                   :num (car (collect-integers-in-line card-num))
                   :winning (collect-integers-in-line left)
                   :picked (collect-integers-in-line right))))

(defun count-matches (card)
  (let ((common (remove-duplicates (intersection (winning card) (picked card)))))
    (length common)))

(defun card-score (card)
  (ash 1 (1- (count-matches card))))

;; Part 2
(defparameter *cards-to-process* nil)
(defparameter *cards-matches* nil)

(defun initialize-cards (file)
  (let ((cards (read-file-as-lines file :parse 'read-card)))
    (setf *cards-to-process* (mapcar 'card-num cards))
    (setf *cards-matches* (coerce (cons nil (mapcar 'count-matches cards))
                          'vector)))) ; extra cons to have card-num = index

(defun answer-ex-4-1 ()
  (reduce '+ (read-file-as-lines "../inputs/input4.txt" :parse 'read-card)
          :key 'card-score))

(defun answer-ex-4-2 ()
  (initialize-cards "../inputs/input4.txt")
  (loop :while *cards-to-process*
        :for card = (pop *cards-to-process*)
        :count 1
        :do (dotimes (i (aref *cards-matches* card))
              (push (+ card i 1) *cards-to-process*))))
