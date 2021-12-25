(in-package #:aoc2021/ex23)

;; Amphipods will never stop on the space immediately outside any room. They can move into that space so long as they immediately continue moving. (Specifically, this refers to the four open spaces in the hallway that are directly above an amphipod starting position.)
;; Amphipods will never move from the hallway into a room unless that room is their destination room and that room contains no amphipods which do not also have that room as their own destination. If an amphipod's starting room is not its destination room, it can stay in that room until it leaves the room. (For example, an Amber amphipod will not move from the hallway into the right three rooms, and will only move into the leftmost room if that room is empty or if it only contains other Amber amphipods.)
;; Once an amphipod stops moving in the hallway, it will stay in that spot until it can move into a room. (That is, once any amphipod starts moving, any other amphipods currently in the hallway are locked in place and will not move again until they can move fully into a room.)

(defparameter *test-23* '("#############"
                          "#...........#"
                          "###B#C#B#D###"
                          "  #A#D#C#A#  "
                          "  #########  "))

(defparameter *test-easy* '("#############"
                            "#...........#"
                            "###A#A#C#D###"
                            "  #B#B#C#D#  "
                            "  #########  "))

(defparameter *rooms-indices* '(2 4 6 8))

(defparameter *cost* #(0 1 10 100 1000))
(defparameter *solution* nil)

;;; Solution 12521

(defun char-to-amphi (char)
  (1+ (- (char-code char)
         (char-code #\A))))

(defun read-rooms (lines)
  (let ((hallway (make-array (- (length (second lines)) 2)))
        rooms)
    (dolist (i *rooms-indices*)
      (push (make-array 2 :initial-contents (list (char-to-amphi (char (fourth lines) (1+ i)))
                                                  (char-to-amphi (char (third lines) (1+ i))))
                          :fill-pointer 2)
            rooms))
    (cons hallway (nreverse rooms))))

(defun make-solution-from-pos (rooms)
  (let ((hallway (car rooms)))
    (setf *solution*
          (list (make-array (array-dimensions hallway))
                (make-array 2 :initial-element 1)
                (make-array 2 :initial-element 2)
                (make-array 2 :initial-element 3)
                (make-array 2 :initial-element 4)))))

(defun init-problem (lines)
  (let ((rooms (read-rooms lines)))
    (make-solution-from-pos rooms)
    rooms))

(defun free-hallway-p (start end hallway &optional include-start)
  (loop :with dir = (signum (- end start))
        :for place = (if include-start start (+ start dir)) :then (+ place dir)
        :always (zerop (aref hallway place))
        :until (= place end)))

(defun free-room-p (room num)
  (case (length room)
    (0 2)
    (1 (and (= (aref room 0) num)
            1))
    (t nil)))

(defun leave-room-k (rooms k)
  (let* ((hallway (car rooms))
         (idx (nth (1- k) *rooms-indices*))
         (copy-rooms (deepcopy rooms))
         (room (nth k copy-rooms)))
    (when (and (plusp (length room))
               (loop :for elt :across room
                     :thereis (/= elt k)))
      (let* ((element (vector-pop room))
             (len (length hallway))
             (depth (if (plusp (length room)) 1 2))
             (cost (aref *cost* element)))
        (append
         ;; Moves to the right while it is possible
         (loop :for i :from (1+ idx) :below len
               :for moves :from (1+ depth)
               :for place = (aref hallway i)
               :while (zerop place)
               :collect (let ((new (deepcopy copy-rooms)))
                          (setf (aref (car new) i) element)
                          (cons new (* moves cost))))
         ;; Moves to the left
         (loop :for i :from (1- idx) :downto 0
               :for moves :from (1+ depth)
               :for place = (aref hallway i)
               :while (zerop place)
               :collect (let ((new (deepcopy copy-rooms)))
                          (setf (aref (car new) i) element)
                          (cons new (* moves cost)))))))))

(defun transfer-room (rooms)
  (let ((hallway (car rooms)))
    (loop :for i :from 1 :to 4
          :for room-idx = (nth (1- i) *rooms-indices*)
          :for room = (nth i rooms)
          :for top = (when (plusp (length room))
                       (aref room (1- (length room))))
          :for dir = (and top (signum (- top room-idx)))
          :for goal-idx = (and top (nth (1- top) *rooms-indices*))
          :when (and top
                     (/= top i)
                     (free-hallway-p room-idx goal-idx hallway t))
            :do
               (let ((depth-goal-idx (free-room-p (nth top rooms) top))
                     (depth-top (if (= 1 (length room)) 2 1)))
                 (when depth-goal-idx
                   (let ((new (deepcopy rooms))
                         (dist (+ depth-goal-idx
                                  depth-top
                                  (abs (- goal-idx room-idx))))
                         (unit-cost (aref *cost* top)))
                     (vector-pop (nth i new))
                     (vector-push top (nth top new))
                     (return (list (cons new (* dist unit-cost))))))))))

(defun %goto-room-element (rooms elt-idx)
  (let* ((hallway (car rooms))
         (element (aref hallway elt-idx))
         (room-idx (nth (1- element) *rooms-indices*))
         (room (nth element rooms))
         (room-free (free-room-p room element))
         (cost (aref *cost* element))
         (depth (if (plusp (length room)) 1 2)))
    (when (and room-free
               (free-hallway-p elt-idx room-idx hallway))
      (let ((new (deepcopy rooms)))
        (setf (aref (car new) elt-idx) 0)
        (vector-push element (nth element new))
        (list (cons new (+ (* (abs (- room-idx elt-idx)) cost)
                           (* depth cost))))))))

(defun goto-room (rooms)
  (loop :with hallway = (car rooms)
        :for elt :across hallway
        :for i :from 0
        :unless (zerop elt)
          :append (%goto-room-element rooms i)))

(defun gen-moves (rooms)
  (or (goto-room rooms)
      (transfer-room rooms)
      (append (loop :for k :from 1 :to 4
                    :append (leave-room-k rooms k)))))

(defun energy-from (rooms &optional show-path)
  (multiple-value-bind (energy parents) (shortest-path #'gen-moves
                                                       rooms
                                                       *solution*
                                                       :test 'equalp)
    (when show-path
      (loop :for v = *solution* :then (gethash v parents)
            :collect v :into pos
            :until (equalp v rooms)
            :finally
               (loop :initially (terpri)
                     :for p = rooms :then q
                     :for q :in (cdr (reverse pos))
                     :when q :do
                       (find-cost p q)
                       (print-rooms q))))
    energy))

(defun find-cost (before after)
  (loop :for (pos . cost) :in (gen-moves before)
        :until (equalp pos after)
        :finally (format t "Cost: ~a~%" cost)))

(defun print-rooms (rooms)
  (destructuring-bind (hallway r1 r2 r3 r4) rooms
    (format t "Rooms:~%")
    (loop :for c :across hallway :do
      (format t "~:[ ~;~a~]" (plusp c) c))
    (format t "~%  ~{~a ~}~%"
            (mapcar (lambda (arr)
                      (case (length arr)
                        (0 " ")
                        (1 " ")
                        (2 (aref arr 1))))
                    (list r1 r2 r3 r4)))
    (format t "  ~{~a ~}~%~%"
            (mapcar (lambda (arr)
                      (case (length arr)
                        (0 " ")
                        ((1 2) (aref arr 0))))
                    (list r1 r2 r3 r4)))))

(defun answer-ex-23-1 ()
  (let* ((lines (read-file-as-lines "../inputs/input23.txt"))
         (rooms (init-problem lines)))
    (energy-from rooms)))

(defun answer-ex-23-2 ())
