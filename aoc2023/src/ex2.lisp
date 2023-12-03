(in-package #:aoc2023/ex2)

;; Part 1
(defun split-game-in-rounds (game)
  (cdr (ppcre:split "[:;] " game))) ; CDR to remove "Game n: " part

(defun parse-round (round)
  (loop :for cube :in (ppcre:split ", " round)
        :for (num col) = (ppcre:split " " cube)
        :collect (cons (read-from-string col) (parse-integer num))))

(defun parse-game (line)
  (mapcar 'parse-round (split-game-in-rounds line)))

(defun amount-colour-drawn (colour round)
  (or (cdr (assoc colour round))
      0))

(defun valid-round-p (round)
  (and (<= (amount-colour-drawn 'red round) 12)
       (<= (amount-colour-drawn 'green round) 13)
       (<= (amount-colour-drawn 'blue round) 14)))

(defun valid-game-p (game)
  (every 'valid-round-p game))

;; Part 2
(defun merge-rounds (round1 round2)
  (loop :for colour :in '(red green blue)
        :for col1 = (amount-colour-drawn colour round1)
        :for col2 = (amount-colour-drawn colour round2)
        :collect (cons colour (max col1 col2))))

(defun minimal-bag-for-game (game)
  (reduce 'merge-rounds game))

(defun power-game (game)
  (reduce '* (minimal-bag-for-game game) :key 'cdr))

(defun answer-ex-2-1 ()
  (loop :for i :from 1
        :for game :in (read-file-as-lines "../inputs/input2.txt" :parse 'parse-game)
        :when (valid-game-p game)
          :sum i))

(defun answer-ex-2-2 ()
  (reduce '+ (read-file-as-lines "../inputs/input2.txt" :parse 'parse-game)
          :key 'power-game))
