(in-package #:aoc2024/ex4)

(defparameter *input* #P"input")
(defparameter *test* #P"test")

(defun collect-word (puzzle size sx sy dx dy)
  (coerce (loop :repeat size
                :for x = sx :then (+ x dx)
                :for y = sy :then (+ y dy)
                :while (array-in-bounds-p puzzle x y)
                :collect (aref puzzle x y))
          'string))

(defun match-xmas-at (puzzle i j di dj)
  (string= (collect-word puzzle 4 i j di dj) "XMAS"))

(defun crossing-mas-at (puzzle i j)
  (let ((first (collect-word puzzle 3 (1- i) (1- j) 1 1))
        (second (collect-word puzzle 3 (1- i) (1+ j) 1 -1))
        (words '("MAS" "SAM")))
    (and (member first words :test 'string=)
         (member second words :test 'string=))))

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


(defun answer-ex-4-2 (file)
  (let ((puzzle (read-file-as-array file))
        (res 0))
    (do-array (i j x puzzle)
      (when (and (char= x #\A)
                 (crossing-mas-at puzzle i j))
        (incf res)))
    res))
