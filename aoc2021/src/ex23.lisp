(in-package #:aoc2021/ex23)

(defparameter *test-23* '("#############"
                          "#...........#"
                          "###B#C#B#D###"
                          "  #A#D#C#A#  "
                          "  #########  "))

(defparameter *rooms-indices* '(2 4 6 8))

(defparameter *cost* #(0 1 10 100 1000))
(defparameter *solution* nil)

(defparameter *unfold-lines* '("  #D#C#B#A#  "
                               "  #D#B#A#C#  "))

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
    (make-array 5 :initial-contents (cons hallway (nreverse rooms))
                  :element-type '(array (integer 0 4)))))

(defun read-rooms-unfold (lines)
  (let ((hallway (make-array (- (length (second lines)) 2)))
        rooms)
    (dolist (i *rooms-indices*)
      (push (make-array 4 :initial-contents (list (char-to-amphi (char (fourth lines)
                                                                       (1+ i)))
                                                  (char-to-amphi (char (second *unfold-lines*)
                                                                       (1+ i)))
                                                  (char-to-amphi (char (first *unfold-lines*)
                                                                       (1+ i)))
                                                  (char-to-amphi (char (third lines)
                                                                       (1+ i))))
                          :fill-pointer 4)
            rooms))
    (make-array 5 :initial-contents (cons hallway (nreverse rooms))
                  :element-type '(array (integer 0 4)))))

(defun make-solution-from-pos (rooms &optional (max-depth 2))
  (let ((hallway (aref rooms 0)))
    (setf *solution*
          (make-array 5 :initial-contents (list (make-array (array-dimensions hallway))
                                                (make-array max-depth :initial-element 1)
                                                (make-array max-depth :initial-element 2)
                                                (make-array max-depth :initial-element 3)
                                                (make-array max-depth :initial-element 4))))))

(defun init-problem (lines &optional (max-depth 2))
  (let ((rooms (if (= 2 max-depth)
                   (read-rooms lines)
                   (read-rooms-unfold lines))))
    (make-solution-from-pos rooms max-depth)
    rooms))

(defun free-hallway-p (start end hallway &optional include-start)
  (loop :with dir = (signum (- end start))
        :for place = (if include-start start (+ start dir)) :then (+ place dir)
        :always (zerop (aref hallway place))
        :until (= place end)))

(defun free-room-p (room num &optional (max-depth 2))
  (let ((len (length room)))
    (when (< len max-depth)
      (loop :for i :below len
            :always (= (aref room i) num)
            :finally (return (- max-depth len))))))

(defun leave-room-k (rooms k &optional (max-depth 2))
  (let* ((hallway (aref rooms 0))
         (idx (nth (1- k) *rooms-indices*))
         (copy-rooms (deepcopy rooms))
         (room (aref copy-rooms k)))
    (when (and (plusp (length room))
               (loop :for elt :across room
                     :thereis (/= elt k)))
      (let* ((element (vector-pop room))
             (len (length hallway))
             (depth (- max-depth (length room)))
             (cost (aref *cost* element)))
        (append
         ;; Moves to the right while it is possible
         (loop :for i :from (1+ idx) :below len
               :for moves :from (1+ depth)
               :for place = (aref hallway i)
               :while (zerop place)
               :unless (member i *rooms-indices*)
                 :collect (let ((new (deepcopy copy-rooms)))
                            (setf (aref (aref new 0) i) element)
                            (cons new (* moves cost))))
         ;; Moves to the left
         (loop :for i :from (1- idx) :downto 0
               :for moves :from (1+ depth)
               :for place = (aref hallway i)
               :while (zerop place)
               :unless (member i *rooms-indices*)
                 :collect (let ((new (deepcopy copy-rooms)))
                            (setf (aref (aref new 0) i) element)
                            (cons new (* moves cost)))))))))

(defun transfer-room (rooms &optional (max-depth 2))
  (let ((hallway (aref rooms 0)))
    (loop :for i :from 1 :to 4
          :for room = (aref rooms i)
          :for room-idx = (nth (1- i) *rooms-indices*)
          :for top = (when (plusp (length room))
                       (aref room (1- (length room))))
          :for dir = (and top (signum (- top room-idx)))
          :for goal-idx = (and top (nth (1- top) *rooms-indices*))
          :when (and top
                     (/= top i)
                     (free-hallway-p room-idx goal-idx hallway t))
            :do
               (let ((depth-goal-idx (free-room-p (aref rooms top) top max-depth))
                     (depth-top (1+ (- max-depth (length room)))))
                 (when depth-goal-idx
                   (let ((new (deepcopy rooms))
                         (dist (+ depth-goal-idx
                                  depth-top
                                  (abs (- goal-idx room-idx))))
                         (unit-cost (aref *cost* top)))
                     (vector-pop (aref new i))
                     (vector-push top (aref new top))
                     (return (list (cons new (* dist unit-cost))))))))))

(defun %goto-room-element (rooms elt-idx &optional (max-depth 2))
  (let* ((hallway (aref rooms 0))
         (element (aref hallway elt-idx))
         (room-idx (nth (1- element) *rooms-indices*))
         (room (aref rooms element))
         (room-free (free-room-p room element max-depth))
         (cost (aref *cost* element))
         (depth (- max-depth (length room))))
    (when (and room-free
               (free-hallway-p elt-idx room-idx hallway))
      (let ((new (deepcopy rooms)))
        (setf (aref (aref new 0) elt-idx) 0)
        (vector-push element (aref new element))
        (list (cons new (+ (* (abs (- room-idx elt-idx)) cost)
                           (* depth cost))))))))

(defun goto-room (rooms &optional (max-depth 2))
  (loop :with hallway = (aref rooms 0)
        :for elt :across hallway
        :for i :from 0
        :unless (zerop elt)
          :append (%goto-room-element rooms i max-depth)))

(defun gen-moves (rooms &optional (max-depth 2))
  (or (goto-room rooms max-depth)
      (transfer-room rooms max-depth)
      (append (loop :for k :from 1 :to 4
                    :append (leave-room-k rooms k max-depth)))))

(defun energy-from (rooms &optional (max-depth 2) show-path)
  (flet ((gen-moves-depth (rooms)
           (gen-moves rooms max-depth)))
    (multiple-value-bind (energy parents) (shortest-path #'gen-moves-depth
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
      energy)))

(defun %test-energy-from (rooms &optional (max-depth 2) show-path)
  (flet ((gen-moves-depth (rooms)
           (gen-moves rooms max-depth)))
    (multiple-value-bind (energy parents) (shortest-path-dec-key #'gen-moves-depth
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
      energy)))

(defun find-cost (before after &optional (max-depth 2))
  (loop :for (pos . cost) :in (gen-moves before max-depth)
        :until (equalp pos after)
        :finally (format t "Cost: ~a~%" cost)))

(defun print-rooms (rooms &optional (max-depth 2))
  (let  ((hallway (aref rooms 0))
         (r1 (aref rooms 1))
         (r2 (aref rooms 2))
         (r3 (aref rooms 3))
         (r4 (aref rooms 4)))
    (format t "Rooms:~%")
    (loop :for c :across hallway :do
      (format t "~:[ ~;~a~]" (plusp c) c))
    (loop :for i :from (1- max-depth) :downto 0 :do
      (format t "~%  ~{~a ~}~%"
              (mapcar (lambda (arr)
                        (let ((len (length arr)))
                          (if (< (1- len) i)
                              " "
                              (aref arr i))))
                      (list r1 r2 r3 r4))))
    (terpri)))

(defun answer-ex-23-1 ()
  (let* ((lines (read-file-as-lines "../inputs/input23.txt"))
         (rooms (init-problem lines)))
    (energy-from rooms 2)))

(defun answer-ex-23-2 ()
  (let* ((lines (read-file-as-lines "../inputs/input23.txt"))
         (rooms (init-problem lines 4)))
    (energy-from rooms 4)))
