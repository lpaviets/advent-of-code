(in-package #:aoc2021/ex9)

(defun parse-input (list)
  (let* ((height (length list))
         (width (length (car list)))
         (array (make-array (list height width))))
    (loop :for line :in list
          :for h ::from 0 :do
            (loop :for c :across line
                  :for val = (- (char-int c) (char-int #\0))
                  :for w :from 0
                  :do (setf (aref array h w) val)))
    array))

(defun valid-pos-p (pos height width)
  (and (<= 0 (car pos) (1- height))
       (<= 0 (cadr pos) (1- width))))

(defun neighbours (i j array)
  (let ((height (array-dimension array 0))
        (width (array-dimension array 1))
        neighbours)
    (dolist (pos `((,(1- i) ,j)
                   (,i ,(1- j))
                   (,i ,(1+ j))
                   (,(1+ i) ,j)))
      (when (valid-pos-p pos height width)
        (push pos neighbours)))
    neighbours))

(defun low-point-p (i j array)
  (let ((neighbours (neighbours i j array)))
    (loop :with val = (aref array i j)
          :for (x y) :in neighbours
          :always (< val (aref array x y)))))

;;; Part 2
(defun next-step (i j array)
  (loop :for (x y) :in (neighbours i j array)
        :for val-neigh = (aref array x y)
        :for min-pos = (list x y) :then (if (< val-neigh (apply 'aref array min-pos))
                                            (list x y)
                                            min-pos)
        :finally (return min-pos)))

(defun find-basin (i j array)
  (loop :for (x y) = (list i j) :then (next-step x y array)
        :until (low-point-p x y array)
        :finally (return (list x y))))

(defun all-basins (array)
  (let ((basins-array (make-array (array-dimensions array))))
    (do-array (i j x array)
      (unless (= 9 x)
        (let ((basin (find-basin i j array)))
          (incf (aref basins-array (first basin) (second basin))))))
    basins-array))

(defun visualize-basins (array)
  (let ((basins-array (make-array (array-dimensions array))))
    (do-array (i j x array)
      (let ((basin (find-basin i j array)))
        (setf (aref basins-array i j) basin)))
    basins-array))

(defun largest-basins (basins how-many)
  (let (large-basins)
    (do-array (i j x basins)
      (when (plusp x)
        (push x large-basins)))
    (subseq (sort large-basins '>) 0 how-many)))

(defun answer-ex-9-1 ()
  (let* ((list (read-file-as-lines "inputs/input9.txt"))
         (array (parse-input list))
         (total-risk 0))
    (do-array (i j x array)
      (when (low-point-p i j array)
        (incf total-risk (1+ x))))
    total-risk))

(defun answer-ex-9-2 ()
  (let* ((list (read-file-as-lines "inputs/input9.txt"))
         (array (parse-input list))
         (basins (all-basins array))
         (large (largest-basins basins 3)))
    (apply '* large)))
