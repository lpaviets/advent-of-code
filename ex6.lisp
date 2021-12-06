(in-package #:aoc2021/ex6)

(defun update-timer-count (array timer &optional (count 1))
  (incf (aref array timer) count))

(defun gen-array (timers)
  (loop :with array = (make-array 9)
        :for timer :in timers
        :do (update-timer-count array timer 1)
        :finally (return array)))

(defun parse-input (line)
  (gen-array (mapcar 'parse-integer (ppcre:split "," line))))

(defun make-timestep (array)
  (loop :with new-array = (make-array 9)
        :for i :from 1 :upto 8
        :for count = (aref array i)
        :when (plusp count)
          :do (update-timer-count new-array (1- i) count)
        :finally (let ((zeroes (aref array 0)))
                   (update-timer-count new-array 8 zeroes)
                   (update-timer-count new-array 6 zeroes)
                   (return new-array))))

(defun update-for-days (array days)
  (dotimes (i days array)
    (setf array (make-timestep array))))

(defun answer-ex-6-1 ()
  (let* ((line (car (read-file-as-lines "inputs/input6.txt")))
         (array (parse-input line)))
    (loop :for c :across (update-for-days array 80) :sum c)))

(defun answer-ex-6-2 ()
  (let* ((line (car (read-file-as-lines "inputs/input6.txt")))
         (array (parse-input line)))
    (loop :for c :across (update-for-days array 256) :sum c)))
