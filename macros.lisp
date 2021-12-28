;;; Advent of code:
;;; Macros

(in-package #:advent-of-code)

(defmacro do-array ((i j x array &optional return) &body body)
  "Iterate over a 2D array.
In the BODY:
I, J are respectively bound to the first and second coordinate at each step
X is bound the array[i][j] := (aref array i j)"
  (let ((garray (gensym)))
    `(let ((,garray ,array))
       (loop :for ,i :below (array-dimension ,garray 0)
             :do
                (loop :for ,j :below (array-dimension ,garray 1)
                      :for ,x = (aref ,garray ,i ,j)
                      :do ,@body))
       ,return)))

(defmacro do-line ((i j &key (step 1)) (x1 y1) (x2 y2) &body body)
  "Iterate over a line, given as a pair of coordinates
In BODY, I and J are bound to the horizontal and vertical coordinate of each
step respectively. They are incremented by STEP at each step, in the right
direction.
Works if the line is \"straight\" or diagonal,. Otherwise, loop infinitely.
Loop infinitely if the STEP does not 'divide' the distance between the starting
point and the ending point."
  (let ((gx1 (gensym)) (gy1 (gensym))
        (gx2 (gensym)) (gy2 (gensym))
        (gstep (gensym))
        (inc-x (gensym)) (inc-y (gensym)))
    `(let* ((,gx1 ,x1) (,gy1 ,y1)
            (,gx2 ,x2) (,gy2 ,y2)
            (,gstep ,step)
            (,inc-x (signum (* ,gstep (- ,gx2 ,gx1))))
            (,inc-y (signum (* ,gstep (- ,gy2 ,gy1)))))
       (do ((,i ,gx1 (+ ,i ,inc-x))
            (,j ,gy1 (+ ,j ,inc-y)))
           ((and (= ,i ,gx2)
                 (= ,j ,gy2)
                 (progn
                   ,@body)))
         ,@body))))
