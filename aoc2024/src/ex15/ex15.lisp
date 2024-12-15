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
(defun left-box-p (c)
  (char= c #\[))

(defun right-box-p (c)
  (char= c #\]))

(defun wide-box-p (c)
  (or (left-box-p c) (right-box-p)))

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
;; Funny corner-case:
;;
;;    []
;;   [][]
;;    []
;;    @
;; moving up.
(defun %move-box-no-check (p1 p2 next1 next2 grid)
  (setf (grid-at next1 grid) (grid-at p1 grid)
        (grid-at next2 grid) (grid-at p2 grid)
        (grid-at p1 grid) #\.
        (grid-at p2 grid) #\.))

(defun move-wide-box (pos dir grid)
  (let* ((other (wide-box-other pos grid))
         (next-pos (grid-pos-in-direction pos dir))
         (next-other (grid-pos-in-direction other dir))
         (all-next (list (grid-at next-pos grid)
                         (grid-at next-other grid))))
    (cond
      ((every 'freep all-next)
       (%move-box-no-check pos other next-pos next-other grid))
      ((not (some 'wallp all-next))
       (move-wide-box next-pos dir grid)
       (move-wide-box next-other dir grid)))
    ;; TODO: continue
    ))

;;; Answers
(defun answer-ex-15-1 (file)
  (destructuring-bind (grid instrs)
      (parse-instructions file)
    (loop :with initial-pos = (find-robot grid)
          :for pos = initial-pos :then next-pos
          :for dir :in instrs
          :for next-pos = (move-robot pos dir grid))
    (gps-coordinates grid)))

(defun answer-ex-15-2 (file))
