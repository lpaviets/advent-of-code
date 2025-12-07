(in-package #:aoc2025/ex7)

(defparameter *input* #P"input")
(defparameter *test* #P"test")

(defun read-splitters (file)
  (read-file-as-array file))

(defun splitter-p (i j splitters)
  (char= #\^ (aref splitters i j)))

(defun initial-pos (splitters)
  (dotimes (j (array-dimension splitters 1))
    (when (char= #\S (aref splitters 0 j))
      (return (list 0 j)))))

(defun total-splits (splitters)
  (let ((counts 0))
    (loop :with tachyons = (list (initial-pos splitters))
          :with h = (array-dimension splitters 0)
          :with tachyons-next = (make-hash-table :test 'equalp)
          :while tachyons ; current particles
          ;; All positions ever reached
          :for (i j) = (pop tachyons)
          :if (and (< i (1- h))
                   (splitter-p (1+ i) j splitters))
            :do (incf counts)
                (dolist (pos (list (list (1+ i) (1+ j))
                                   (list (1+ i) (1- j))))
                  (unless (gethash pos tachyons-next)
                    (setf (gethash pos tachyons-next) t)
                    (push pos tachyons)))
          :else :if (< i (1- h))
                  :do (let ((pos (list (1+ i) j)))
                        (unless (gethash pos tachyons-next)
                          (setf (gethash pos tachyons-next) t)
                          (push pos tachyons))))
    counts))

;; Idea : count how many splits there are from there to the end
;; Easy to initialize on bottommost row
;; For the inductive step : simply look at the rules !

;; Minor assumption on the input: no splitters on the last row
;; This is not important, just simplifies the code
;; Another minor assumption : no splitter on the edge, so we avoid
;; some bound checks
(defun splits-until-end (splitters)
  (let ((counts (make-array (array-dimensions splitters)
                            :initial-element 0)))
    (loop :with (h w) = (array-dimensions splitters)
          ;; Last row : no splits possible, let it at 0
          :for i :from (- h 2) :downto 0
          :do (loop :for j :from 0 :to (1- w)
                    :if (splitter-p (1+ i) j splitters)
                      :do (setf (aref counts i j)
                                (+ (aref counts (1+ i) (1- j))
                                   (aref counts (1+ i) (1+ j))
                                   1))
                    :else
                      :do (setf (aref counts i j) (aref counts (1+ i) j))))
    counts))

(defun answer-ex-7-1 (file)
  (total-splits (read-splitters file)))

(defun answer-ex-7-2 (file)
  (let* ((splitters (read-splitters file))
         (initial-pos (initial-pos splitters))
         (splits (splits-until-end splitters)))
    ;; Need to add 1, as we count splits (a single split gives rise to two
    ;; particles)
    (1+ (grid-at initial-pos splits))))
