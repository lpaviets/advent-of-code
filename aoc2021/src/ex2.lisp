(in-package #:aoc2021/ex2)

(defun total-horiz-depth (list)
  (loop :with horiz = 0
        :with depth = 0
        :for (dir . val) :in list
        :do
           (cond
             ((string= dir "forward") (incf horiz val))
             ((string= dir "up") (decf depth val))
             ((string= dir "down") (incf depth val)))
        :finally (return (* horiz depth))))

(defun total-horiz-depth-aim (list)
  (loop :with horiz = 0
        :with depth = 0
        :with aim = 0
        :for (dir . val) :in list
        :do
           (cond
             ((string= dir "forward")
              (incf horiz val)
              (incf depth (* aim val)))
             ((string= dir "up")
              (decf aim val))
             ((string= dir "down")
              (incf aim val)))
        :finally (return (* horiz depth))))

(defun answer-ex-2-1 ()
  (let ((list (read-file-as-lines "../inputs/input2.txt" :parse 'split-word-int)))
    (total-horiz-depth list)))

(defun answer-ex-2-2 ()
  (let ((list (read-file-as-lines "../inputs/input2.txt" :parse 'split-word-int)))
    (total-horiz-depth-aim list)))
