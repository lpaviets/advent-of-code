(in-package #:aoc2021/ex25)

(defun cucumber-to-int (line)
  (loop :for cucumber :across line
        :collect
        (case cucumber
          (#\. 0)
          (#\> 1)
          (#\v 2))))

(defun step-east (array)
  (let ((new (deepcopy array))
        (width (array-dimension array 1))
        change)
    (do-array (i j x array)
      (when (= x 1)
        (let ((right (mod (1+ j) width)))
          (when (zerop (aref array i right))
            (setf change t)
            (setf (aref new i j) 0
                  (aref new i right) 1)))))
    (values new change)))

(defun step-south (array)
  (let ((new (deepcopy array))
        (height (array-dimension array 0))
        change)
    (do-array (i j x array)
      (when (= x 2)
        (let ((south (mod (1+ i) height)))
          (when (zerop (aref array south j))
            (setf change t)
            (setf (aref new i j) 0
                  (aref new south j) 2)))))
    (values new change)))

(defun step-complete (array)
  (multiple-value-bind (new change) (step-east array)
    (multiple-value-bind (new2 change2) (step-south new)
      (values new2 (or change change2)))))

(defun steps-before-stop (array)
  (loop :for (new change) = (list array t)
          :then (multiple-value-list (step-complete new))
        :for i :from 0
        :while change
        :finally (return i)))

(defun answer-ex-25-1 ()
  (let* ((lines (read-file-as-lines "../inputs/input25.txt"
                                    :parse 'cucumber-to-int))
         (cucumbers (make-array (list (length lines)
                                      (length (car lines)))
                                :initial-contents lines
                                :element-type '(integer 0 2))))
    (steps-before-stop cucumbers)))

(defun answer-ex-25-2 ())
