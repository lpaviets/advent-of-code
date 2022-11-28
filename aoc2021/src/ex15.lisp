(in-package #:aoc2021/ex15)

(defun fill-lowest-risk-heap (array)
  "Dijkstra algorithm. Uses a binary heap for prio queue"
  (let ((height (array-dimension array 0))
        (width (array-dimension array 1))
        (target (mapcar '1- (array-dimensions array)))
        (distance (make-array (array-dimensions array)
                              :initial-element most-positive-fixnum))
        (parent (make-array (array-dimensions array))))

    (let ((heap (heap:make-heap '< :key 'third))) ; x y distance
      (setf (aref distance 0 0) 0)        ; Initialize
      (heap:heap-push (list 0 0 0) heap)

      (loop :while (heap:heap-peek heap)
            :for i :from 0
            :for best = (heap:heap-pop heap)
            :for (x y d) = best
            :for curr-dist = (aref distance x y)
            :when (= d curr-dist) :do
              (loop :for (i j) :in (neighbours x y array)
                    :for dist = (+ (aref distance x y)
                                   (aref array i j))
                    :when (< dist (aref distance i j))
                      :do (setf (aref distance i j) dist)
                          (heap:heap-push (list i j dist) heap)
                          (setf (aref parent i j) best))
            :until (equal best target)))

    (loop :for (x y) = (list (1- height) (1- width))
            :then (aref parent x y)
          :until (equal (list x y) '(0 0))
          :sum (aref array x y))))


(defun build-big-array (array)
  (let* ((height (array-dimension array 0))
         (width (array-dimension array 1))
         (big-array (make-array (list (* 5 height)
                                      (* 5 width)))))
    (do-array (i j risk big-array)
      (let* ((diag (+ (truncate i height)
                      (truncate j width)))
             (x (mod i height))
             (y (mod j width))
             (val (aref array x y)))
        (setf (aref big-array i j)
              (1+ (mod (1- (+ diag val)) 9)))))
    big-array))

(defun answer-ex-15-1 ()
  (let* ((list (read-file-as-lines "../inputs/input15.txt"))
         (array (read-array list)))
    (fill-lowest-risk-heap array)))

(defun answer-ex-15-2 ()
  (let* ((list (read-file-as-lines "../inputs/input15.txt"))
         (array (read-array list))
         (big-array (build-big-array array)))
    (fill-lowest-risk-heap big-array)))
