(in-package #:aoc2025/ex4)

(defparameter *input* #P"input")
(defparameter *test* #P"test")

(defun read-rolls (file)
  (read-file-as-array file))

(defun rollp (c)
  (char= #\@ c))

(defun count-neighbouring-rolls (pos rolls)
  (loop :for nghb-pos :in (grid-neighbours pos rolls :diagonal t)
        :count (rollp (grid-at nghb-pos rolls))))

(defun accessible-roll (pos rolls)
  (when (rollp (grid-at pos rolls))
    (< (count-neighbouring-rolls pos rolls) 4)))

(defun make-adjacent-blocks-grid (rolls)
  "Returns a grid of the same dimension as ROLLS.

The entry at position X, Y is NIL if there are no block here, and otherwise the
number of neighbouring rolls in ROLLS."
  (let ((neighbours (make-array (array-dimensions rolls) :initial-element nil)))
    (do-array (i j entry rolls)
      (when (rollp entry)
        (let ((pos (list i j)))
          (setf (aref neighbours i j)
                (count-neighbouring-rolls pos rolls)))))
    neighbours))

(defun all-accessible-rolls (rolls)
  (let ((res nil))
    (do-array (i j x rolls res)
      (let ((pos (list i j)))
        (when (and (rollp (grid-at pos rolls))
                   (< (count-neighbouring-rolls pos rolls) 4))
          (push pos res))))))

(defun solve (rolls)
  ;; Initially, count adjacent rolls, and collect the accessible ones
  (let ((neighbours (make-adjacent-blocks-grid rolls))
        (remaining (all-accessible-rolls rolls)))

    ;; Whenever we process an accessible roll:
    ;; - We decrease its rolls' neighbours count by 1
    ;; - If any of those neighbours become accessible:
    ;;    - Add it to the queue to process if not already seen
    ;;    - mark it as seen
    (dolist (pos remaining)
      (setf (grid-at pos neighbours) nil))
    (loop :while remaining
          :for pos = (pop remaining)
          :count 1
          :do (dolist (nghb (grid-neighbours pos rolls :diagonal t))
                (when (grid-at nghb neighbours)
                  (destructuring-bind (x y) nghb
                    (decf (aref neighbours x y))
                    (when (< (aref neighbours x y) 4)
                      (push nghb remaining)
                      (setf (grid-at nghb neighbours) nil))))))))

(defun answer-ex-4-1 (file)
  (let ((rolls (read-rolls file))
        (res 0))
    (do-array (i j x rolls res)
      (when (accessible-roll (list i j) rolls)
        (incf res)))))

(defun answer-ex-4-2 (file)
  (let ((rolls (read-rolls file)))
    (solve rolls)))
