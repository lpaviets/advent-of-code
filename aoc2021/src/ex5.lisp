(in-package #:aoc2021/ex5)

(defun straight-line-p (start end)
  (or (= (car start) (car end))
      (= (cadr start) (cadr end))))

(defun keep-straight (list)
  (loop :for (start end) :in list
        :when (straight-line-p start end)
          :collect (list start end)))

(defun parse-coordinates (line)
  (ppcre:register-groups-bind ((#'parse-integer x1)
                               (#'parse-integer y1)
                               (#'parse-integer x2)
                               (#'parse-integer y2))
      ("\(\\d+\),\(\\d+\) -> \(\\d+\),\(\\d+\)" line)
    (list (list x1 y1) (list x2 y2))))

(defun max-coordinates (list)
  (loop :for ((x1 y1) (x2 y2)) :in list
        :for max-x = 0 :then (max max-x x1 x2)
        :for max-y = 0 :then (max max-y y1 y2)
        :finally (return (list (1+ max-x) (1+ max-y)))))

(defun count-overlaps (list)
  (let ((grid (make-array (max-coordinates list)))
        (count 0))
    (loop :for ((x1 y1) (x2 y2)) :in list :do
          (do-line (i j) (x1 y1) (x2 y2)
            (incf (aref grid i j))))
    (do-array (i j x grid)
      (when (<= 2 x)
        (incf count)))
    count))

;;; Part 2
(defun diag-line-p (start end)
  (let ((dx (- (car end) (car start)))
        (dy (- (cadr end) (cadr start))))
    (or (= dx dy)
        (= dx (- dy)))))

(defun keep-straight-and-diag (list)
  (loop :for (start end) :in list
        :when (or (straight-line-p start end)
                  (diag-line-p start end))
          :collect (list start end)))

(defun answer-ex-5-1 ()
  (let ((list (read-file-as-lines "../inputs/input5.txt"
                                  :parse 'parse-coordinates)))
    (count-overlaps (keep-straight list))))

(defun answer-ex-5-2 ()
  (let ((list (read-file-as-lines "../inputs/input5.txt"
                                  :parse 'parse-coordinates)))
    (count-overlaps (keep-straight-and-diag list))))
