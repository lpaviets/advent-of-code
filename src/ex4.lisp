(in-package #:aoc2021/ex4)

(defparameter *numbers-draw* nil)
(defparameter *bingo-boards* nil)
(defparameter *bingo-boards-marks* nil)
(defparameter *bingo-size* 5)

(defun reset-boards ()
  (setf *numbers-draw* nil
        *bingo-boards* nil
        *bingo-boards-marks* nil))

(defun parse-bingo (list)
  (loop :for line :in list
        :for i :below *bingo-size*
        :collect (mapcar 'parse-integer (ppcre:split " +" (string-trim " " line)))
          :into list-grid
        :finally (return (make-array (list *bingo-size* *bingo-size*)
                                     :initial-contents list-grid))))

(defun parse-input (list)
  (loop :initially (setf *numbers-draw* (mapcar 'parse-integer
                                                (ppcre:split "," (car list))))
        :for bingo-list = (cddr list) :then (nthcdr (1+ *bingo-size*) bingo-list)
        :while bingo-list :do
          (push (parse-bingo bingo-list)
                *bingo-boards*)
          (push (make-array (list *bingo-size* *bingo-size*)
                            :initial-element nil)
                *bingo-boards-marks*)
        :finally (setf *bingo-boards* (nreverse *bingo-boards*))))

(defun draw-number (n)
  (loop :for board :in *bingo-boards*
        :for marked :in *bingo-boards-marks*
        :do (do-array (i j x board)
              (when (= x n)
                (setf (aref marked i j) t)))))

(defun row-complete (board i)
  (loop :for j :below (array-dimension board 1)
        :always (aref board i j)))

(defun column-complete (board j)
  (loop :for i :below (array-dimension board 0)
        :always (aref board i j)))

(defun board-complete (board)
  (or (loop :for i :below (array-dimension board 0)
             :thereis (row-complete board i))
       (loop :for j :below (array-dimension board 1)
             :thereis (column-complete board j))))

(defun find-bingo-score ()
  (loop :for board :in *bingo-boards*
        :for marked :in *bingo-boards-marks*
        :thereis (and (board-complete marked)
                      (let ((score 0))
                        (do-array (i j x board)
                          (unless (aref marked i j)
                            (incf score x)))
                        score))))

(defun play-bingo-game ()
  (loop :for num :in *numbers-draw*
        :do (draw-number num)
        :thereis (let ((score (find-bingo-score)))
                   (and score (* score num)))))

;;; Part 2

(defun count-completed-boards ()
  (loop :for board :in *bingo-boards*
        :for marked :in *bingo-boards-marks*
        :count (board-complete marked)))

(defun find-last-board ()
  (loop :with num-boards = (length *bingo-boards*)
        :for num :in *numbers-draw*
        :do (draw-number num)
        :until (= (1- num-boards) (count-completed-boards))
        :finally (return (loop :for board :in *bingo-boards*
                               :for marked :in *bingo-boards-marks*
                               :thereis (and (not (board-complete marked))
                                             board)))))

(defun play-bingo-game-last ()
  (let* ((last-board (find-last-board))
         (*bingo-boards* (list last-board))
         (*bingo-boards-marks* (list (make-array (array-dimensions last-board)
                                                 :initial-element nil))))
    (play-bingo-game)))

(defun answer-ex-4-1 ()
  (let ((list (read-file-as-lines "../inputs/input4.txt")))
    (reset-boards)
    (parse-input list)
    (play-bingo-game)))

(defun answer-ex-4-2 ()
  (let ((list (read-file-as-lines "../inputs/input4.txt")))
    (reset-boards)
    (parse-input list)
    (play-bingo-game-last)))
