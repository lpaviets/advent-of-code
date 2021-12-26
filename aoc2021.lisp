;;;; 2021.lisp

(in-package #:aoc2021)

;;; Parsing
;; Reading from files
(defun read-file-as-lines (filename &key parse)
  "Read file into a list of lines."
  (with-open-file (in filename)
    (loop :for line = (read-line in nil nil)
          :while line
          :collect (if parse
                       (funcall parse line)
                       line))))

(defun read-file-one-line (filename &key parse)
  "Read the first line of FILENAME, applying PARSE to it if non-NIL"
  (with-open-file (in filename)
    (let ((line (read-line in nil nil)))
      (if parse
          (funcall parse line)
          line))))

(defun read-file-as-integers (filename)
  (read-file-as-lines filename :parse 'parse-integer))

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

;; Other parsing utilities

(defun split-word-int (line)
  (ppcre:register-groups-bind (word (#'parse-integer int))
      ("\(\\w+\) \(\\d+\)" line)
    (cons word int)))

(defun coma-separated-int-line (line)
  (mapcar 'parse-integer (ppcre:split " *, *" line)))

(defun parse-digit (char)
  (and (char<= #\0 char #\9)
       (- (char-int char) (char-int #\0))))

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

(defun %shuffle (list len acc)
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
  (%shuffle list 0 nil))

;;; Algorithms

;;; Dijkstra
;;; Algo:
;;; Initialization:
;;; 1. Q <- empty
;;; 2. for each v in V(G): dist[v] <- +inf
;;; 3. Q.insert(source, 0)
;;;
;;; Algo:
;;; while Q is not empty do:
;;;   (u, k) <- delete_min(Q)
;;;   if k == dist[u]:
;;;     for each (u, v) in E(G) do:
;;;       if dist[u] + weight(u, v) < dist[v]:
;;;         Q.insert(v, dist[u] + weight(u, v))
;;;         dist[v] <- dist[u] + weight(u, v)

(defgeneric shortest-path (edges source target &key test)
  (:documentation "Shortest path from SOURCE to TARGET in the graph G determined
by EDGES.
Comparison between vertices is done using TEST"))

(defmethod shortest-path (edges source target &key (test 'eql))
  "EDGES is expected to be a function of one element, a vertex (of the
same type as SOURCE and TARGET), and return a list of cons cells,
whose car is an adjacent vertex and whose cdr is the edge's weight.
For example:
(funcall EDGES SOURCE) -> ((u1 . w1) (u2 . w2)) ... (un . wn))
where the ui's are exactly the vertices adjacent to SOURCE."
  (let  ((distance (make-hash-table :test test))
         (queue (heap:make-heap #'< :key #'cdr))
         (parent (make-hash-table :test test)))
    (loop
      :initially
         (setf (gethash source distance) 0)
         (heap:heap-push (cons source 0) queue)
      :while (heap:heap-peek queue)
      :for best = (heap:heap-pop queue)
      :for (vertex . queue-dist) = best
      :for curr-dist = (or (gethash vertex distance)
                           most-positive-fixnum)
      :when (= queue-dist curr-dist)
        :do (loop :for edge :in (funcall edges vertex)
                  :for (other . weight) = edge
                  :for other-new-dist = (+ queue-dist weight)
                  :when (< other-new-dist (or (gethash other distance)
                                              most-positive-fixnum))
                    :do
                       (setf (gethash other distance) other-new-dist)
                       (heap:heap-push (cons other other-new-dist) queue)
                       (setf (gethash other parent) vertex))
      :until (funcall test vertex target)
      :finally (return (values (gethash target distance)
                               parent)))))

(defmethod shortest-path ((edges array) (source integer) (target integer) &key (test 'eql))
  "EDGES is given as a 2D array, representing the adjacency matrix of the graph
(aref EDGES i j) is the weight of the oriented edge between I and J
SOURCE and TARGET are INTEGER, corresponding to valid indices of the array"
  (flet ((fun-edges (vertex)
           (loop :for j :below (array-dimension edges 1)
                 :for weight = (aref edges vertex j)
                 :collect (cons j weight))))
    (shortest-path #'fun-edges source target :test test)))


;; ;; Other implementation: uses a heap with a 'decrease-key' operation
;; ;; Way better space-wise, not necessarily the case speed-wise
(defmethod shortest-path-dec-key (edges source target &key (test 'eql))
  "EDGES is expected to be a function of one element, a vertex (of the
same type as SOURCE and TARGET), and return a list of cons cells,
whose car is an adjacent vertex and whose cdr is the edge's weight.
For example:
(funcall EDGES SOURCE) -> ((u1 . w1) (u2 . w2)) ... (un . wn))
where the ui's are exactly the vertices adjacent to SOURCE."
  (let  ((distance (make-hash-table :test test))
         (parent (make-hash-table :test test)))
    (declare (special distance))
    (flet ((get-dist (x)
             (or (gethash x distance)
                 most-positive-fixnum)))
      (loop
        :with queue = (heap+:make-heap #'< :key #'get-dist)
        :initially
           (setf (gethash source distance) 0)
           (heap+:heap-push source queue)
        :while (heap+:heap-peek queue)
        :for vertex = (heap+:heap-pop queue)
        :for curr-dist = (get-dist vertex)

        :do (loop :for edge :in (funcall edges vertex)
                  :for (other . weight) = edge
                  :for other-new-dist = (+ curr-dist weight)
                  :when (< other-new-dist (get-dist other))
                    :do
                       (setf (gethash other distance) other-new-dist)
                       (heap+:heap-push other queue)
                       (setf (gethash other parent) vertex))
        :until (funcall test vertex target)
        :finally (hash-table-count distance)
           (return (values (gethash target distance)
                           parent))))))

;; DFS

(defun %dfs (edges pending &key at-vertex (test 'eql) target random)
  (let ((visited (make-hash-table :test test)))
    (loop :while pending
          :for (x parent cost) = (pop pending)
          :unless (gethash x visited) :do
            (loop :for (edge . cost) :in (if random
                                             (shuffle (funcall edges x))
                                             (funcall edges x))
                  :do
                     (push (list edge x cost) pending))
            (setf (gethash x visited) t)
            (when at-vertex
              (funcall at-vertex x parent cost))
          :until (and target (funcall test x target)))))

(defun dfs (edges source &key at-vertex (test 'eql) target random)
  "Depth-First-Search of the graph determined by EDGES.
EDGES is a function of one argument, a vertex, and return a list of cons cells
(NEIGHBOUR . EDGE-WEIGHT)
SOURCE is the initial vertex from which to perform the DFS
AT-VERTEX is a function of three argument, the vertex being visited, its parent,
 and the cost of the edge used to reach it.
TEST is how vertices are compared
If TARGET is non-NIL, the DFS stops after reaching a vertex equal (as of TEST)
to it.
RANDOM determines whether the edges are searched in a deterministic way or not"
  (%dfs edges (list (list source))
        :at-vertex at-vertex
        :test test
        :target target
        :random random))

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

(defun print-array (array)
  (loop :for i :below (array-dimension array 0) :do
    (loop :for j :below (array-dimension array 1) :do
      (format t "~a" (aref array i j)))
    (format t "~%"))
  array)

(defun print-hash (object &optional (stream t))
  (format stream "#HASH{~{~{(~s : ~s)~}~%~^ ~}}"
          (loop :for key :being :the :hash-keys :of object
                  :using (:hash-value value)
                :collect (list key value))))
