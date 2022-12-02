(in-package #:aoc2022/ex2)

;;; Simplify: work MOD 3
(defun sym-to-action (sym)
  (case sym
    ((a x) 0)
    ((b y) 1)
    ((c z) 2)))

(defun game-to-actions (game)
  (mapcar 'sym-to-action game))

(defun action-score (action)
  (1+ action))

(defun outcome-score (game)
  (destructuring-bind (other me)
      game
    (* 3 (mod (1+ (- me other)) 3))))

(defun score-from-game (game)
  (+ (outcome-score game) (action-score (second game))))

(defun score-from-pair (pair)
  (score-from-game (game-to-actions pair)))

(defun find-action (other me)
  (mod (case me
         (x (1- other))
         (y other)
         (z (1+ other))
         (otherwise (error "Invalid actions :~S, ~S" other me)))
       3))

(defun game-outcome-to-actions (game)
  (destructuring-bind (other me)
      game
    (let* ((other (sym-to-action other))
           (me (find-action other me)))
      (list other me))))

(defun score-from-game-outcome (game)
  (score-from-game (game-outcome-to-actions game)))

(defun answer-ex-2-1 ()
  (let ((games (read-file-as-sexprs "../inputs/input2")))
    (reduce #'+ games :key #'score-from-pair)))

(defun answer-ex-2-2 ()
  (let ((games (read-file-as-sexprs "../inputs/input2")))
    (reduce #'+ games :key #'score-from-game-outcome)))
