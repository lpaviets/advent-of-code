;;;; 2021.lisp

(in-package #:aoc2021)

;;; Parsing

(defun read-file-as-lines (filename &key (parse 'identity))
  "Read file into a list of lines."
  (with-open-file (in filename)
    (loop :for line = (read-line in nil nil)
          :while line
          :collect (funcall parse line))))


(defun read-file-as-integers (filename)
  (read-file-as-lines filename :parse 'parse-integer))

(defun split-word-int (line)
  (ppcre:register-groups-bind (word (#'parse-integer int))
      ("\(\\w+\) \(\\d+\)" line)
    (cons word int)))

(defun coma-separated-int-line (line)
  (mapcar 'parse-integer (ppcre:split " *, *" line)))

;;; Utilities

(defun flip (x y &optional (comp 'equal) (keep-others t))
  "Return a function of one argument which swaps X and Y
If KEEP-OTHERS is non-NIL, this function acts like `identity' on other elements
Otherwise, elements that are not X nor Y are mapped to Y"
  (lambda (z)
    (cond
      ((funcall comp x z) y)
      ((funcall comp y z) x)
      (keep-others z)
      (t y))))

(defmacro do-array ((i j x array) &body body)
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
                      :do ,@body)))))

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
