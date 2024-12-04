(in-package #:aoc2024/ex4)

(defparameter *input* #P"input")
(defparameter *test* #P"test")

(defun match-xmas-at (puzzle i j di dj)
  (loop :for c :across "XMAS"
        :for x = i :then (+ x di)
        :for y = j :then (+ y dj)
        :while (array-in-bounds-p puzzle x y)
        :always (char= c (aref puzzle x y))))

(defun answer-ex-4-1 (file)
  (let ((puzzle (read-file-as-array file))
        (res 0))
    (do-array (i j x puzzle)
      (when (char= x #\X)
        (loop :for di :in '(-1 0 1)
              :do (loop :for dj :in '(-1 0 1)
                        :unless (equalp '(0 0) `(,di ,dj))
                          :when (match-xmas-at puzzle i j di dj)
                            :do (incf res 1)))))
    res))


(defun answer-ex-4-2 (file))
