;;; Advent of code:
;;; Utilities

(in-package #:advent-of-code)

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

(defun range (m &optional n (step 1))
  (loop :for i = (if n m 0) :then (+ i step)
        :until (= i (or n m))
        :collect i))

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

(defun sublists-length (list n)
  "List of all the sublists of LIST of length N"
  (cond
    ((= 1 n) (mapcar 'list list))
    ((null list) nil)
    (t
     (append (sublists-length (cdr list) n)
             (mapcar (lambda (x)
                       (cons (car list) x))
                     (sublists-length (cdr list) (1- n)))))))

(defun neighbours (i j array &key diagonal self)
  "List of positions of the neighbours of (I J) in ARRAY
If DIAGONAL is non-nil, includes the diagonally adjacent neighbours
If SELF is non-nil, include (I J) too"
  (let (neighbours)
    (dotimes (x 3)
      (dotimes (y 3)
        (let ((next-x (1- (+ x i)))
              (next-y (1- (+ y j))))
          (when (and (or (not (= x y 1)) self)
                     (and (or diagonal
                              (= next-x i)
                              (= next-y j)))
                     (array-in-bounds-p array next-x next-y))
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

(defgeneric deepcopy (thing)
  (:documentation "Recursively creates a copy of THING.")
  (:method ((thing t)) thing))

(defmethod deepcopy ((thing list))
  (mapcar #'deepcopy thing))

(defmethod deepcopy ((thing array))
  (loop :with new = (make-array (array-dimensions thing)
                                :element-type (array-element-type thing)
                                :adjustable (adjustable-array-p thing)
                                :fill-pointer (and (array-has-fill-pointer-p thing)
                                                   (fill-pointer thing)))
        :for i :below (array-total-size thing) :do
          (setf (row-major-aref new i)
                (deepcopy (row-major-aref thing i)))
        :finally (return new)))

(defmethod deepcopy ((thing hash-table))
  (let ((table (make-hash-table :size (hash-table-size thing)
                                :test (hash-table-test thing))))
    (maphash (lambda (k v)
               (setf (gethash (deepcopy k) table)
                     (deepcopy v)))
             thing)
    table))

(defun shuffle* (list len acc)
  (declare (type list list acc)
           (type fixnum len)
           (optimize (speed 3)))
  (if (endp list)
      acc
      (let* ((rand (random (+ len 1)))
             (new-acc (append (butlast acc rand)
                              (list (car list))
                              (last acc rand))))
        (%shuffle (cdr list) (+ len 1) new-acc))))

(defun shuffle (list)
  (shuffle* list 0 nil))
