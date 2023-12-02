(in-package #:aoc2022/ex12)

(defun parse-input (file)
  (let (start
        end
        (grid (read-file-as-array file nil)))
    (do-array (i j x grid)
      (case x
        (#\S
         (setf start (list i j))
         (setf (aref grid i j) 0))
        (#\E
         (setf end (list i j))
         (setf (aref grid i j) 25))
        (t
         (setf (aref grid i j) (- (parse-integer (string x) :radix 36) 10)))))
    (list grid start end)))

(declaim (inline at))
(defun at (grid v)
  (aref grid (first v) (second v)))

;;; Small change needed for part 2:
;;; Instead of returning the vertices to which we can go, we return the
;;; vertices from which we can come.
;;; Part1 is then solved starting from E and trying to go to S
;;; We can come from a vertex with elevation at least one below us
(defun grid-neighbours (grid v)
  (destructuring-bind (i j) v
    (let ((at-v (at grid v)))
      (remove-if (lambda (u)
                   (< 1 (- at-v (at grid u))))
                 (neighbours i j grid)))))

(defun make-grid-edges-function (grid)
  (lambda (v)
    (loop :for nghb :in (grid-neighbours grid v)
          :collect (cons nghb 1))))

(defun answer-ex-12-1 ()
  (destructuring-bind (grid start end)
      (parse-input "../inputs/input12")
    (let ((distance (bfs (make-grid-edges-function grid) end :target start :test 'equal)))
      (gethash start distance))))

(defun answer-ex-12-2 ()
  (destructuring-bind (grid start end)
      (parse-input "../inputs/input12")
    (declare (ignore start))
    (bfs (make-grid-edges-function grid) end
         :target 0
         :test 'equal
         :target-test (lambda (v elevation) (= (at grid v) elevation)))))
