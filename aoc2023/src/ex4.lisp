(in-package #:aoc2023/ex4)

(defclass card ()
  ((num :initarg :num :accessor card-num)
   (winning :initarg :winning :accessor winning)
   (picked :initarg :picked :accessor picked)))

(defun make-card (num left right)
  (make-instance 'card :num num :winning left :picked right))

(defun read-card (card)
  (destructuring-bind (card-num left right)
      (ppcre:split "[:|]" card)
    (make-card (car (collect-integers-in-line card-num))
               (collect-integers-in-line left)
               (collect-integers-in-line right))))

(defmacro with-card ((num winning picked) card &body body)
  `(with-accessors ((,num card-num)
                    (,winning winning)
                    (,picked picked))
       ,card
     ,@body))

(defun count-matches (card)
  (with-card (num winning picked) card
    (let ((common (remove-duplicates (intersection winning picked))))
      (length common))))

(defun card-score (card)
  (let ((matches (count-matches card)))
    (if (zerop matches) 0 (ash 1 (1- matches)))))

;; Part 2
(defparameter *cards-to-process* nil)
(defparameter *cards* nil)

(defun initialize-cards (file)
  (let ((cards (read-file-as-lines file :parse 'read-card)))
    (setf *cards-to-process* (mapcar 'card-num cards))
    (setf *cards* (coerce (cons nil (mapcar 'count-matches cards))
                          'vector)))) ; extra cons to have card-num = index

(defun answer-ex-4-1 ()
  (reduce '+ (read-file-as-lines "../inputs/input4.txt" :parse 'read-card)
          :key 'card-score))

(defun answer-ex-4-2 ()
  (initialize-cards "../inputs/input4.txt")
  (loop :while *cards-to-process*
        :for card = (pop *cards-to-process*)
        :count 1
        :do (dotimes (i (aref *cards* card))
              (push (+ card i 1) *cards-to-process*))))
