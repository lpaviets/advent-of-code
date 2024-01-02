(in-package #:aoc2023/ex23)

(defparameter *print-replace* '((:right . #\>)
                                (:left . #\<)
                                (:down . #\v)
                                (:up . #\^)))

(defun parse-grid (file)
  (let ((grid (read-file-as-array file)))
    (do-array (i j x grid)
      (setf (aref grid i j)
            (case x
              (#\> :right)
              (#\< :left)
              (#\v :down)
              (#\^ :up)
              (t   x))))
    grid))

(defun neighbours (grid pos)
  (let ((x (grid-at pos grid)))
    (cond
      ((eql x #\#) nil)
      ((keywordp x) (list (grid-pos-in-direction pos x)))
      (t (loop :for next :in (grid-neighbours pos grid :walls #\#)
               :for next-val = (grid-at next grid)
               :unless (and (keywordp next-val)
                            (equalp pos (grid-pos-in-direction next next-val)))
                 :collect next)))))

(defun find-start (grid)
  (loop :for j :below (grid-width grid)
        :for x = (aref grid 0 j)
        :thereis (and (eql x #\.) (list 0 j))))

(defun find-end (grid)
  (loop :with max-height = (1- (grid-height grid))
        :for j :below (grid-width grid)
        :for x = (aref grid max-height j)
        :thereis (and (eql x #\.) (list max-height j))))

;; Part 2
(defun make-neighbours-p2 (start grid)
  (lambda (pos)
    (when (or (equalp pos start)
              (not (intersection-p pos grid)))
      (loop :for next :in (grid-neighbours pos grid :walls #\#)
            :collect (cons next 0)))))

(defun intersection-p (pos grid)
  (let ((nghb (grid-neighbours pos grid :walls #\#)))
    (<= 3 (length nghb))))

(defun find-intersections (grid)
  (let ((acc (list (find-end grid) (find-start grid))))
    (do-array (i j x grid (nreverse acc))
      (let ((pos (list i j)))
        (when (and (not (eql x #\#))
                   (intersection-p pos grid))
          (push pos acc))))))

;; Completely incorrect algorithm ! Works  though ...
;;
;; Problem: if we have a kind of "double path", we /still/ remember
;; the shortest one due to the bfs ... For example:
;;
;; ---X---------Y----
;;    |         |
;;    ⌞_________⌟
;;
;; Both X and Y are intersections, but the BFS will say that they are
;; at distance |Y-X| while there can be an arbitrary detour.
(defun make-distances-intersections (grid)
  (let ((intersections (find-intersections grid)))
    (loop :with distances = (make-hash-table :test #'equalp)
          :with start = (find-start grid)
          :with end = (find-end grid)
          :for int :in intersections
          :for int-distances = (nth-value 1 (bfs (make-neighbours-p2 int grid)
                                                 int
                                                 :test #'equalp))
          :for int-other-distances = (let (others)
                                       (do-hash (x v int-distances)
                                         (unless (eq x int)
                                           (when (or (intersection-p x grid)
                                                     (equalp start x)
                                                     (equalp end x))
                                             (push (cons x v) others))))
                                       (nreverse others))
          :collect (cons int int-other-distances))))

;; Max distance between START and END without using any of USED
;; Path is the path /already taken/ to go to start.
;; If we are at the end: return the path taken
;; Otherwise: amongst the next possible intersections, pick the one
;; with the largest
;;
;; SLOW: but we cannot easily cache stuff, as the USED list is changing.
(defun max-distance-from (start end distances used cur-dist cur-path)
  (if (equalp start end)
      (cons cur-dist cur-path)
      (loop :with max-dist = 0
            :with max-path = nil
            :for (next . dist) :in (cdr (assoc start distances :test #'equalp))
            :unless (member next used :test #'equalp)
              :do (destructuring-bind (next-dist . next-path)
                      (max-distance-from next
                                         end
                                         distances
                                         (cons next used)
                                         (+ dist cur-dist)
                                         (cons start cur-path))
                    (when (< max-dist next-dist)
                      (setf max-dist next-dist
                            max-path next-path)))
            :finally (return (cons max-dist max-path)))))

(defun answer-ex-23-1 ()
  (let* ((grid (parse-grid "../inputs/input23.txt"))
         (edges (lambda (x)
                  (loop :for y :in (neighbours grid x)
                        :collect (cons y 0))))
         (start (find-start grid))
         (end (find-end grid))
         (ordered (topological-sort edges
                                    start
                                    :test #'equalp))
         (real-edges (make-hash-table :test #'equalp)))
    ;; Pretend we have a directed graph
    (loop :for (x . after-x) :on ordered
          :do (loop :for edge :in (funcall edges x)
                    :for (y . cost) = edge
                    :when (member y after-x :test #'equalp)
                      :do (pushnew edge (gethash x real-edges))))
    (gethash end (longest-path (lambda (x)
                                 (gethash x real-edges))
                               start
                               :test #'equalp))))

(defun answer-ex-23-2 ()
  (let* ((grid (parse-grid ;; "test.txt"
                           "../inputs/input23.txt"
                           ))
         (start (find-start grid))
         (end (find-end grid))
         (distances (make-distances-intersections grid)))
    (max-distance-from start end distances (list start) 0 nil)))
