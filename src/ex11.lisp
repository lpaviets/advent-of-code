(in-package #:aoc2021/ex11)

(defun flash-step (array)
  (let ((count 0)
        (continue t)
        flashed)
    (do-array (i j x array)
      (incf (aref array i j)))
    (loop :while continue
          :do (setf continue nil)
              (do-array (i j x array)
                (when (and (> x 9)
                           (not (member (list i j) flashed :test 'equal)))
                  (setf continue t)
                  (incf count)
                  (push (list i j) flashed)
                  (dolist (pos (neighbours i j array :diagonal t))
                    (incf (aref array (first pos) (second pos))))))

          :finally (dolist (pos flashed)
                     (setf (aref array (first pos) (second pos)) 0)))
    count))

(defun all-steps (array)
  (loop :repeat 100
        :for count = (flash-step array)
        :sum count))

(defun first-sync (array)
  (loop :with octopi = (apply '* (array-dimensions array))
        :for count = (flash-step array)
        :for i :from 1
        :thereis (and count
                      (= count octopi)
                      i)))

(defun answer-ex-11-1 ()
  (let* ((list (read-file-as-lines "../inputs/input11.txt"))
         (array (read-array list)))
    (all-steps array)))

(defun answer-ex-11-2 ()
    (let* ((list (read-file-as-lines "../inputs/input11.txt"))
         (array (read-array list)))
      (first-sync array)))
