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

(defun read-array (list &optional (digits t))
  "Read a 2D-array. If DIGITS is non-nil, parses elements as digits"
  (loop :with array = (make-array (list (list-length list)
                                        (length (car list))))
        :for line :in list
        :for i :from 0 :do
          (loop :for c :across line
                :for j :from 0
                :for val = (if digits
                               (- (char-int c) (char-int #\0))
                               c)
                :do
                   (setf (aref array i j) val))
        :finally (return array)))

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

(defun permutations (n)
  "List of all the permutations of the integers between 0 and n included"
  (if (zerop n)
      (list '(0))
      (loop :with perms = (permutations (1- n))
            :for perm :in perms
            :append
            (loop :repeat (1+ n)
                  :for (beg end) = (list nil perm)
                    :then (list (cons (car end) beg)
                                (cdr end))
                  :collect (append beg (list n) end)))))


(defun valid-position (i j h w)
  (and (<= 0 i (1- h))
       (<= 0 j (1- w))))

(defun neighbours (i j array &key diagonal self)
  "List of positions of the neighbours of (I J) in ARRAY
If DIAGONAL is non-nil, includes the diagonally adjacent neighbours
If SELF is non-nil, include (I J) too"
  (let ((height (array-dimension array 0))
        (width (array-dimension array 1))
        neighbours)
    (dotimes (x 3)
      (dotimes (y 3)
        (let ((next-x (1- (+ x i)))
              (next-y (1- (+ y j))))
          (when (and (or (not (= x y 1)) self)
                     (and (or diagonal
                              (= next-x i)
                              (= next-y j)))
                     (valid-position next-x
                                     next-y
                                     height
                                     width))
            (push (list next-x next-y)
                  neighbours)))))
    neighbours))

(defun extremum (list predicate &key key)
  "Return the value of the element for the comparison operator PREDICATE in
LIST. If KEY is non-NIL, PREDICATE is applied to (KEY ELEMENT) instead of
ELEMENT at each step"
  (when list
    (let* ((element (first list))
           (val (if key (funcall key element) element)))
      (mapc (lambda (x)
              (let ((val-x (if key (funcall key x) x)))
                (when (funcall predicate val-x val)
                  (psetf element x
                         val val-x))))
            list)
      element)))

(defun extremum-array (array predicate &key key)
  "Return the position in the 2D-array ARRAY for which the value of
(KEY (ARRAY[pos] &optional POS) is minimal for the order induced by PREDICATE
KEY is a function of 1 mandatory argument (the value of ARRAY at position POS)
and one optional one (the position, given as a list of length 2)"
  (let* ((pos '(0 0))
         (val-pos (aref array 0 0))
         (val-min (if key (funcall key val-pos '(0 0)) val-pos)))
    (do-array (i j x array)
      (let ((val-x (if key (funcall key x (list i j)) x)))
        (when (funcall predicate val-x val-min)
          (psetf pos (list i j)
                 val-min val-x))))
    pos))

;;; Useful macros

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
