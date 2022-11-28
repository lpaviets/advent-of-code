(in-package #:aoc2021/ex20)

(declaim (optimize (debug 3) (safety 0) (speed 0)))

(defparameter *rules* nil)

(defun parse-rules (line)
  (setf *rules* line))

(defun unrestricted-neighbours (x y)
  (loop :for i :from (1- x) :upto (1+ x)
        :append (loop :for j :from (1- y) :upto (1+ y)
                      :collect (list i j))))

(defun get-unrestricted-neighbours (x y array)
  (loop :for (i j) :in (unrestricted-neighbours x y)
        :collect (if (and (array-in-bounds-p array i j)
                          (char= (aref array i j) #\#))
                     1
                     0)))

(defun convoluted-value (x y array)
  (let* ((binary-str (format nil "~{~a~}" (get-unrestricted-neighbours x y array)))
         (index (parse-integer binary-str :radix 2)))
    (char *rules* index)))

(defun swap-padding (array)
  (flet ((rev (c)
           (if (char= c #\#) #\. #\#)))
   (let* ((h (array-dimension array 0))
          (w (array-dimension array 1)))
     (loop :for j :below w :do
       (setf (aref array 0 j) (rev (aref array 0 j))
             (aref array (1- h) j) (rev (aref array (1- h) j))))

     (loop :for i :from 1 :below (1- h) :do
       (setf (aref array i 0) (rev (aref array i 0))
             (aref array i (1- w)) (rev (aref array i (1- w))))))))

(defun pad-array (array value)
  (let ((new (make-array (list (+ 2 (array-dimension array 0))
                               (+ 2 (array-dimension array 1)))
                         :initial-element value)))
    (do-array (i j x array new)
      (setf (aref new (1+ i) (1+ j)) x))
    new))

(defun make-convolution-step (array &optional (steps 1))
  "Assume that rule(0) = 1, rule(512) = 0"
  (loop :with new
        :for result = (pad-array array #\.)
          :then (progn
                  (setf result (pad-array result pad))
                  (let* ((h (array-dimension result 0))
                         (w (array-dimension result 1))
                         (new (make-array (list h w) :initial-element pad)))
                    (do-array (i j x result)
                      (when (and (< 0 i (1- h))
                                 (< 0 j (1- w)))
                        (setf (aref new i j) (convoluted-value i j result))))
                    (swap-padding new)
                    new))

        :for step :from 0 :below steps
        :for pad = (if (zerop (mod step 2)) #\. #\#)
        :finally (return result)))

(defun count-lit-pixels (array)
  (let ((count 0)
        (h (array-dimension array 0))
        (w (array-dimension array 1)))
    (do-array (i j x array)
      (when (and (<= 1 i (1- h))
                 (<= 1 j (1- w))
                 (char= x #\#))
        (incf count)))
    count))

(defun test-ex-20-1 ()
  (let* ((lines '("..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..###..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#..#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#......#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#.....####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.......##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#"
                  ""
                  "#..#."
                  "#...."
                  "##..#"
                  "..#.."
                  "..###")
                ;; (read-file-as-lines "../inputs/input20.txt")
                )
         (array (read-array (cddr lines) nil)))
    (parse-rules (car lines))
    (print-array (make-convolution-step))))

(defun answer-ex-20-1 ()
  (let* ((lines (read-file-as-lines "../inputs/input20.txt"))
         (array (read-array (cddr lines) nil)))
    (parse-rules (car lines))
    (count-lit-pixels (make-convolution-step array 2))))

(defun answer-ex-20-2 ()
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (let* ((lines (read-file-as-lines "../inputs/input20.txt"))
         (array (read-array (cddr lines) nil)))
    (parse-rules (car lines))
    (count-lit-pixels (make-convolution-step array 50))))
