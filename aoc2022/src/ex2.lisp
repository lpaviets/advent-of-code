(in-package #:aoc2022/ex2)

;;; Simplify: work MOD 3
(defun sym-to-action (sym)
  (ecase sym
    ((a x) 0)
    ((b y) 1)
    ((c z) 2)))

;;; PAIR is a list of symbols, as read from the file
;;; GAME is a list of actions (here, integers mod 3)
(defun pair-to-game (pair)
  (mapcar 'sym-to-action pair))

(defun action-score (action)
  (1+ action))

(defun outcome-score (game)
  (destructuring-bind (other me)
      game
    (* 3 (mod (1+ (- me other)) 3))))

(defun score-from-game (game)
  (+ (outcome-score game) (action-score (second game))))

(defun score-from-pair (pair)
  (score-from-game (pair-to-game pair)))

(defun find-action (other me)
  (mod (ecase me
         (x (1- other))
         (y other)
         (z (1+ other)))
       3))

(defun pair-to-game/outcome (pair)
  (destructuring-bind (other me)
      pair
    (let* ((other (sym-to-action other))
           (me (find-action other me)))
      (list other me))))

(defun score-from-pair/outcome (pair)
  (score-from-game (pair-to-game/outcome pair)))

(defun answer-ex-2-1 ()
  (let ((games (read-file-as-sexprs "../inputs/input2")))
    (reduce #'+ games :key #'score-from-pair)))

(defun answer-ex-2-2 ()
  (let ((games (read-file-as-sexprs "../inputs/input2")))
    (reduce #'+ games :key #'score-from-pair/outcome)))
