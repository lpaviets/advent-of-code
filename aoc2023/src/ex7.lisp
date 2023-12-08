(in-package #:aoc2023/ex7)

(defparameter *card-order* '(#\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\T #\J #\Q #\K #\A))
(defparameter *card-order-joker* '(#\J
                                   #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
                                   #\T #\Q #\K #\A))

(defun hand-signature (hand)
  (loop :for c :in *card-order*
        :for count = (count c hand)
        :when (plusp count)
          :collect (list count c) :into signature
        :finally (return (list (sort (mapcar 'car signature) '>)
                               signature))))

(defun compare-cards (a b &optional (order *card-order*))
  (< (position a order) (position b order)))

(defun lexicographic< (seq1 seq2 &optional (predicate #'<))
  (let ((seq1 (coerce seq1 'list))
        (seq2 (coerce seq2 'list)))
    (loop :for (a . rest1) :on seq1
          :for (b . rest2) :on seq2
          :when (funcall predicate b a)
            :do (return nil)
          :when (funcall predicate a b)
            :do (return t)
          :finally (return (if rest2 t nil)))))

(defun compare-signature (sig-1 sig-2)
  (lexicographic< (car sig-1) (car sig-2)))

(defun compare-hands (hand-1 hand-2)
  (let ((sig-1 (hand-signature hand-1))
        (sig-2 (hand-signature hand-2)))
    (or (compare-signature sig-1 sig-2)
        (and (equalp (car sig-1) (car sig-2))
             (lexicographic< hand-1 hand-2 'compare-cards)))))

(defun read-hand (line)
  (destructuring-bind (hand score)
      (ppcre:split " " line)
    (list (hand-signature hand) hand (parse-integer score))))

;; Part 2
(defun hand-signature-with-joker (hand)
  (loop :with jokers = (count #\J hand)
        :for c :in (cdr *card-order-joker*) ; no joker
        :for count = (count c hand)
        :when (plusp count)
          :collect (list count c) :into signature
        :finally (let ((result (sort (mapcar 'car signature) '>)))
                   (if result          ; if there are non-jokers cards
                       (incf (car result) jokers)
                       (setf result '(5)))
                   (return (list result
                                 (cons (list jokers #\J) signature))))))

(defun compare-hands-with-joker (hand-1 hand-2)
  (let ((sig-1 (hand-signature-with-joker hand-1))
        (sig-2 (hand-signature-with-joker hand-2)))
    (or (compare-signature sig-1 sig-2)
        (and (equalp (car sig-1) (car sig-2))
             (let ((*card-order* *card-order-joker*))
               (lexicographic< hand-1 hand-2 'compare-cards))))))

(defun answer-ex-7-1 ()
  (let ((hands (read-file-as-lines "../inputs/input7.txt" :parse 'read-hand)))
    (setf hands (sort hands 'compare-hands :key 'second))
    (loop :for (sig hand score) :in hands
          :for i :from 1
          :sum (* i score))))

(defun answer-ex-7-2 ()
  (let ((hands (read-file-as-lines "../inputs/input7.txt" :parse 'read-hand)))
    (setf hands (sort hands 'compare-hands-with-joker :key 'second))
    (loop :for (sig hand score) :in hands
          :for i :from 1
          :sum (* i score))))
