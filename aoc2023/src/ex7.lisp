(in-package #:aoc2023/ex7)

(defparameter *card-order* '(#\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\T #\J #\Q #\K #\A))

(defun compare-cards (a b)
  (< (position a *card-order*) (position b *card-order*)))

(defun hand-signature (hand)
  (loop :for c :in *card-order*
        :for count = (count c hand)
        :when (plusp count)
          :collect (cons count c) :into signature
        :finally (return (list (sort (mapcar 'car signature) '>)
                               signature))))

(defun five-kind (signature)
  (equalp (car signature) '(5)))

(defun four-kind (signature)
  (equalp (car signature) '(4 1)))

(defun full-house (signature)
  (equalp (car signature) '(3 2)))

(defun three-kind (signature)
  (equalp (car signature) '(3 1 1)))

(defun two-pairs (signature)
  (equalp (car signature) '(2 2 1)))

(defun one-pair (signature)
  (equalp (car signature) '(2 1 1 1)))

(defun compare-signature (sig-1 sig-2)
  (loop :for a :in (car sig-1)
        :for b :in (car sig-2)
        :always (<= a b)
        :thereis (< a b)))

(defun lexico-compare-hands (hand-1 hand-2)
  (loop :for a :across hand-1
        :for b :across hand-2
        :always (char= a b)
        :thereis (compare-cards a b)))

(defun compare-hands (hand-1 hand-2)
  (let ((sig-1 (hand-signature hand-1))
        (sig-2 (hand-signature hand-2)))
    (or (compare-signature sig-1 sig-2)
        (and (equalp (car sig-1) (car sig-2))
             (lexico-compare-hands hand-1 hand-2)))))

(defun read-hand (line)
  (destructuring-bind (hand score)
      (ppcre:split " " line)
    (list (hand-signature hand) hand (parse-integer score))))

(defun answer-ex-7-1 ()
  (let ((hands (read-file-as-lines "../inputs/input7.txt" :parse 'read-hand)))
    (setf hands (sort hands 'compare-hands :key 'second))
    (loop :for (sig hand score) :in hands
          :for i :from 1
          :sum (* i score))))

(defun answer-ex-7-2 ())
