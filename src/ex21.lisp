(in-package #:aoc2021/ex21)

(defclass player ()
  ((pos
    :accessor pos
    :initarg :pos
    :type (integer 1 10))
   (score
    :accessor score
    :initarg score
    :initform 0
    :type integer)))

(defparameter *player-1* nil)
(defparameter *player-2* nil)

(defun new-pos (pos move)
  (1+ (mod (1- (+ pos move)) 10)))

(defmethod make-move ((player player) move)
  (with-accessors ((pos pos) (score score)) player
    (let ((new (new-pos pos move)))
      (setf pos new )
      (incf score new))))

(defun init-pos (line)
  (parse-integer line :start 28))

(defun init-players (lines)
  (setf *player-1* (make-instance 'player :pos (init-pos (first lines))))
  (setf *player-2* (make-instance 'player :pos (init-pos (second lines)))))

(defun deterministic-game ()
  (loop :for dice = 1 :then (1+ (mod (+ 2 dice) 100))
        :for total-turns :from 3 :by 3
        :for turn = 1 :then (- 1 turn)
        :for move = (+ (* 3 dice) 3)
        :if (= turn 1)
          :do (make-move *player-1* move)
        :else
          :do (make-move *player-2* move)
        :thereis (or (and (<= 1000 (score *player-1*))
                          (* total-turns
                             (score *player-2*)))
                     (and (<= 1000 (score *player-2*))
                          (* total-turns
                             (score *player-1*))))))

;;; Part 2

(defparameter *three-throws* #(0 0 0 1 3 6 7 6 3 1))
(defparameter *paths-to-victory* nil
  "Key ((S1 P1) (S2 P2) T), contain the number of paths from this position to the
victory of either player, as a cons cell.
S1 and S2 are the players score, P1 and P2 their position on the board, and T
is either 0 or 1 according to whose turn it is to play")

(defun min-score-from-pos (pos)
  (cond
    ((= pos 1) 4)
    ((= pos 9) 2)
    ((= pos 10) 3)
    (t 1)))

(defun quantum-paths-from (s1 p1 s2 p2 turn)
  (cond
    ((>= s1 21) '(1 . 0))
    ((>= s2 21) '(0 . 1))
    (t
     (let* ((key (list s1 p1
                       s2 p2
                       turn))
            (val (gethash key *paths-to-victory*)))
       (or val
           (let ((res (loop :for move :from 3 :to 9
                            :for paths = (aref *three-throws* move)
                            :for new-pos = (new-pos (if (zerop turn) p1 p2)
                                                    move)
                            :for new-score = (+ new-pos (if (zerop turn) s1 s2))
                            :for new-key = (if (zerop turn)
                                               (list new-score new-pos s2 p2 1)
                                               (list s1 p1 new-score new-pos 0))
                            :for (win-1 . win-2) = (apply 'quantum-paths-from new-key)
                            :sum (* paths win-1) :into w1
                            :sum (* paths win-2) :into w2
                            :finally (return (cons w1 w2)))))
             (setf (gethash key *paths-to-victory*) res)))))))

(defun answer-ex-21-1 ()
  (let ((lines (read-file-as-lines "../inputs/input21.txt")))
    (init-players lines)
    (deterministic-game)))

(defun answer-ex-21-2 ()
  (let ((lines (read-file-as-lines "../inputs/input21.txt")))
    (init-players lines)
    (setf *paths-to-victory* (make-hash-table :test 'equal))
    (destructuring-bind (w1 . w2)
        (quantum-paths-from (score *player-1*)
                            (pos *player-1*)
                            (score *player-2*)
                            (pos *player-2*)
                            0)
      (max w1 w2))))
