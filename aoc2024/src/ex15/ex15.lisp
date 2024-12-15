(in-package #:aoc2024/ex15)

(defparameter *input* #P"input")
(defparameter *test* #P"test")

(defun convert-dir (dir)
  (ecase dir
    (#\^ :up)
    (#\> :right)
    (#\v :down)
    (#\< :left)))

(defun parse-instructions (file)
  (destructuring-bind (grid instructions)
      (read-file-as-lines-blocks file)
    (list (read-array grid)
          (map 'list 'convert-dir (apply 'concatenate 'string instructions)))))

(defun robotp (c)
  (char= c #\@))

(defun boxp (c)
  (char= c #\O))

(defun wallp (c)
  (char= c #\#))

(defun freep (c)
  (char= c #\.))

(defun find-robot (grid)
  (do-array (i j x grid)
    (when (robotp x)
      (return-from find-robot (list i j)))))

;;; Returns a single position POS, or NIL:
;;; - If NIL, don't move
;;; - If POS: move the bot 1 cell in direction DIR, and place a box at POS
(defun push-boxes-to (pos dir grid)
  (loop :for stop-at = (grid-pos-in-direction pos dir)
          :then (grid-pos-in-direction stop-at dir)
        :while (boxp (grid-at stop-at grid)) ; can't go out: walls all around
        :finally (return (and (freep (grid-at stop-at grid))
                              stop-at))))

(defun move-robot (pos dir grid)
  (let ((stop-at (push-boxes-to pos dir grid)))
    (if stop-at
        (let ((next (grid-pos-in-direction pos dir)))
          (setf (grid-at stop-at grid) #\O
                (grid-at pos grid) #\.
                (grid-at next grid) #\@)
          next)
        pos)))

(defun gps-coordinates (grid)
  (let ((res 0))
    (do-array (i j x grid)
      (when (boxp x)
        (incf res (+ (* 100 i) j))))
    res))

;;; P2: need to redefine a lot of stuff
(defun parse-wide-instructions (file)
  (destructuring-bind (grid instructions)
      (read-file-as-lines-blocks file)
    (let* ((height (length grid))
           (width (length (first grid)))
           (wide-grid (make-array (list height (* 2 width)))))
      (loop :for i :from 0
            :for line :in grid
            :do (loop :for j :from 0
                      :for c :across line
                      :for left = (if (boxp c) #\[ c)
                      :for right = (ecase c
                                     ((#\. #\#) c)
                                     (#\O #\])
                                     (#\@ #\.))
                      :do (setf (aref wide-grid i (* 2 j)) left
                                (aref wide-grid i (1+ (* 2 j))) right)))
      (list wide-grid
            (map 'list 'convert-dir (apply 'concatenate 'string instructions))))))

(defun left-box-p (c)
  (char= c #\[))

(defun right-box-p (c)
  (char= c #\]))

(defun wide-box-p (c)
  (or (left-box-p c) (right-box-p c)))

(defun wide-box-other (pos grid)
  (let* ((box (grid-at pos grid)))
    (cond
      ((left-box-p box) (grid-pos-in-direction pos :right))
      ((right-box-p box) (grid-pos-in-direction pos :left))
      (t (error "Not a box")))))

;; Algo to move:
;; - If free: move
;; - If wall: don't move, say it
;; - If box(es): move them, then move
;;
;; - Difficulty: moving left/right does not follow the same rules as up/down
;; - Good thing: left/right "algo" from P1 works

;; Returns the first free position after a bunch of boxes, or NIL if we hit a
;; wall first.
(defun push-wide-boxes-to (pos dir grid)
  (loop :for stop-at = (grid-pos-in-direction pos dir)
          :then (grid-pos-in-direction stop-at dir)
        :while (wide-box-p (grid-at stop-at grid)) ; can't go out: walls all around
        :finally (return (and (freep (grid-at stop-at grid))
                              stop-at))))


;; Return T if something changed, NIL otherwise
(defun move-wide-boxes-horizontally (pos dir grid)
  (let ((stop-at (push-wide-boxes-to pos dir grid)))
    (when stop-at
      (let ((prev #\.)
            (next-prev nil))
        (destructuring-bind (y1 x1) pos
          (destructuring-bind (y2 x2) stop-at
            (do-line (i j) (y1 x1) (y2 x2)
              ;; TODO: why do rotatef/psetf not work ??
              (setf next-prev (aref grid i j)
                    (aref grid i j) prev
                    prev next-prev)))))
      (setf (grid-at pos grid) #\.)
      t)))

;; Funny corner-case:
;;
;;    []
;;   [][]
;;    []
;;    @
;; moving up.
(defun %move-box-vertically-no-check (p1 p2 next1 next2 grid)
  (setf (grid-at next1 grid) (grid-at p1 grid)
        (grid-at next2 grid) (grid-at p2 grid)
        (grid-at p1 grid) #\.
        (grid-at p2 grid) #\.)
  t)

;; Try to move a box. Return T if something changed in the config, NIL
;; otherwise.

(defun move-wide-boxes-vertically (pos dir grid)
  (when (wide-box-p (grid-at pos grid))
    (let* ((other (wide-box-other pos grid))
           (next-pos (grid-pos-in-direction pos dir))
           (next-other (grid-pos-in-direction other dir))
           (all-next (list (grid-at next-pos grid)
                           (grid-at next-other grid))))
      (cond
        ;; All free: move directly
        ((every 'freep all-next)
         (%move-box-vertically-no-check pos other next-pos next-other grid))
        ;; No walls, some boxes to push: try to move them sequentially, then if
        ;; anything changed, try again. Expensive but easy.
        ((not (some 'wallp all-next))
         (let ((moved-1 (move-wide-boxes-vertically next-pos dir grid))
               (moved-2 (move-wide-boxes-vertically next-other dir grid)))
           (when (or moved-1 moved-2)
             (move-wide-boxes-vertically pos dir grid))))
        (t nil)))))

(defun move-wide-boxes (pos dir grid)
  (if (member dir '(:up :down))
      (move-wide-boxes-vertically pos dir grid)
      (move-wide-boxes-horizontally pos dir grid)))

(defun %move-robot-no-check (from to grid)
  (setf (grid-at from grid) #\.
        (grid-at to grid) #\@)
  to)

;; Updates GRID an returns the new bot position
(defun move-robot-wide-boxes (pos dir grid)
  (let* ((next-pos (grid-pos-in-direction pos dir))
         (next-val (grid-at next-pos grid)))
    (cond
      ((freep next-val)
       (%move-robot-no-check pos next-pos grid))
      ((wide-box-p next-val)
       (if (move-wide-boxes next-pos dir grid)
           (%move-robot-no-check pos next-pos grid)
           pos))
      (t pos))))

(defun gps-wide-coordinates (grid)
  (let ((res 0))
    (do-array (i j x grid)
      (when (left-box-p x)
        (incf res (+ (* 100 i) j))))
    res))

;;; Answers
(defun answer-ex-15-1 (file)
  (destructuring-bind (grid instrs)
      (parse-instructions file)
    (loop :with initial-pos = (find-robot grid)
          :for pos = initial-pos :then next-pos
          :for dir :in instrs
          :for next-pos = (move-robot pos dir grid))
    (gps-coordinates grid)))

;; Test: 9021
;; Final grid:
;; ####################
;; ##[].......[].[][]##
;; ##[]...........[].##
;; ##[]........[][][]##
;; ##[]......[]....[]##
;; ##..##......[]....##
;; ##..[]............##
;; ##..@......[].[][]##
;; ##......[][]..[]..##
;; ####################

(defun answer-ex-15-2 (file &optional debug)
  (destructuring-bind (grid instrs)
      (parse-wide-instructions file)
    (when debug (print-array grid))
    (loop :with initial-pos = (find-robot grid)
          :for pos = initial-pos :then next-pos
          :for dir :in instrs
          :for next-pos = (move-robot-wide-boxes pos dir grid)
          :do (when debug (format t "~6%Move ~A:~%" dir)
                    (print-array grid)
                    (sleep 0.05)))
    (when debug (print-array grid))
    (gps-wide-coordinates grid)))
