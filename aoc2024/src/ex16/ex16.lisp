(in-package #:aoc2024/ex16)

(defparameter *input* #P"input")
(defparameter *test* #P"test")

(defun find-reindeer (maze)
  (grid-find #\S maze))

(defun find-exit (maze)
  (grid-find #\E maze))

(defun wallp (x)
  (char= x #\#))

(defun parse-maze (file)
  (let* ((maze (read-file-as-array file))
         (reindeer (find-reindeer maze))
         (exit (find-exit maze)))
    (setf (grid-at reindeer maze) #\.
          (grid-at exit maze) #\.)
    (values maze reindeer exit)))

;; In the cache cell (I J):
;; Store an alist, (:DIR (EDGE-1 EDGE-2 ...))
(defun %add-edge (cache pos init-dir end-dir maze cost)
  (let ((nghb (grid-pos-in-direction pos end-dir)))
    (unless (wallp (grid-at nghb maze))
      (push (cons (list nghb end-dir) cost)
            (cdr (assoc init-dir (aref cache (first pos) (second pos))))))))

(defun orthogonal-dirs (dir)
  (ecase dir
    ((:up :down) '(:left :right))
    ((:left :right) '(:up :down))))

(defun make-graph-from-maze (maze)
  (let ((cache (make-array (array-dimensions maze) :initial-element nil)))
    (do-array (i j x maze)
      (setf (aref cache i j)
            (list (list :up)
                  (list :left)
                  (list :down)
                  (list :right)))
      (unless (wallp x)
        (dolist (dir '(:up :left :down :right))
          (let ((pos (list i j)))
            (%add-edge cache pos dir dir maze 1)
            (dolist (ortho (orthogonal-dirs dir))
              (%add-edge cache pos dir ortho maze 1001))))))
    (lambda (pos-dir)
      (destructuring-bind (pos dir) pos-dir
        (cdr (assoc dir (aref cache (first pos) (second pos))))))))

(defun iterate-path (from parents fun)
  (loop :for pos = from :then (gethash from parents)
        :while pos
        :do (funcall fun pos)))

(defun all-on-shortest-path ((edges function) source target &key (test 'eql))
  "EDGES is expected to be a function of one element, a vertex (of the
same type as SOURCE and TARGET), and return a list of cons cells, whose car is
an adjacent vertex and whose cdr is the edge's weight. For example:
(funcall EDGES SOURCE) -> ((u1 . w1) (u2 . w2)) ... (un . wn))
where the ui's are exactly the vertices adjacent to SOURCE.

TARGET can also be a function of one argument, a vertex, returning T if this
vertex is a possible target."
  (let  ((distance (make-hash-table :test test))
         (queue (heap:make-heap #'< :key #'cdr))
         (parent (make-hash-table :test test))
         (target-p (if (functionp target)
                       (lambda (v)
                         (funcall target v))
                       (lambda (v)
                         (funcall test v target)))))
    (loop
      :initially
         (setf (gethash source distance) 0)
         (heap:heap-push (cons source 0) queue)
      :while (heap:heap-peek queue)
      :for best = (heap:heap-pop queue)
      :for (vertex . queue-dist) = best
      :for curr-dist = (or (gethash vertex distance)
                           most-positive-fixnum)
      ;; Need to save the parent everytime we visit a new cell, in case it falls
      ;; on a shortest path
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
      :until (funcall target-p vertex)
      :finally (return (values (gethash vertex distance)
                               parent
                               vertex)))))

(defun answer-ex-16-1 (file)
  (multiple-value-bind (maze reindeer exit)
      (parse-maze file)
    (let ((start (list reindeer :right)))
      (shortest-path (make-graph-from-maze maze)
                     start
                     (lambda (pos-dir)
                       (equalp exit (first pos-dir)))
                     :test 'equalp))))

(defun answer-ex-16-2 (file))
