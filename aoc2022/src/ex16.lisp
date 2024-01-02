(in-package #:aoc2022/ex16)

(defparameter *valves* nil)
(defparameter *valves/useful* nil)
(defparameter *shortest-paths* nil)
(defparameter *shortest-paths/useful* nil)
(defparameter *AA* nil)

(defun parse-line (line)
  (destructuring-bind (valve flow . neighbours)
      (ppcre:all-matches-as-strings "\\d+|[A-Z]{2}" line)
    (list (intern valve)
          (parse-integer flow)
          (mapcar #'intern neighbours))))

(defun parse-input (file)
  (let* ((lines (read-file-as-lines file :parse #'parse-line))
         (sym-to-num (loop :with table = (make-hash-table :test 'eq)
                           :for (sym . rest) :in lines
                           :for i :from 0
                           :do (setf (gethash sym table) i)
                           :finally (return table))))
    (flet ((sym-to-num (sym)
             (gethash sym sym-to-num)))
      (setf *AA* (sym-to-num 'AA))
      (setf *valves* (loop :with table = (make-hash-table :test 'eql)
                           :for (sym flow nghb) :in lines
                           :do (setf (gethash (sym-to-num sym) table)
                                     (list flow (mapcar #'sym-to-num nghb)))
                           :finally (return table))))))

(defun valve-flow (valve)
  (first (gethash valve *valves*)))

(defun valve-flow/useful (valve)
  (valve-flow (useful-valve->valve valve)))

(defun valve-neighbours (valve)
  (second (gethash valve *valves*)))

(defun distance (from to)
  (aref *shortest-paths* from to))

(defun distance/useful (from to)
  (aref *shortest-paths/useful* from to))

(defun useful-valve->valve (valve)
  (aref *valves/useful* valve))

(defun build-useful-valves ()
  (setf *valves/useful*
        (coerce
         (let ((useful-valves (loop :for k :being :the :hash-key :of *valves*
                                      :using (:hash-value v)
                                    :when (plusp (car v))
                                      :collect k)))
           (sort useful-valves #'> :key #'valve-flow))
         'vector)))

(defun build-shortest-paths ()
  (setf *shortest-paths* nil)
  (let* ((n (hash-table-count *valves*))
         (matrix (make-array (list n n) :initial-element nil)))
    (loop :for valve :being :the :hash-keys :of *valves*
          :for i :from 0
          :do (dolist (next (valve-neighbours valve))
                (setf (aref matrix valve next) 1)))
    (setf *shortest-paths* (shortest-path-all-to-all matrix)))

  (setf *shortest-paths/useful*
        (let* ((n (length *valves/useful*))
               (matrix (make-array (list n n))))
          (loop :for i :from 0
                :for u :across *valves/useful*
                :do (loop :for j :from 0
                          :for v :across *valves/useful*
                          :do (setf (aref matrix i j)
                                    (distance u v))))
          matrix)))

(defun init (file)
  (parse-input file)
  (build-useful-valves)
  (build-shortest-paths))

;;;; Debugging
(defun replay-sol (sol)
  (loop :for pos = *AA* :then valve
        :for valve :in sol
        :for flow = (valve-flow valve)
        :for current-time = (distance pos valve)
          :then (+ 1 current-time (distance pos valve))
        :for valve-contrib = (* flow (- 29 current-time))
        :do (format t "Valve ~2A: arriving at time ~2A~
, with flow ~A (contribution: ~4A)~%"
                    valve current-time flow valve-contrib)
        :sum valve-contrib :into total-flow
        :finally (format t "~&Total flow: ~4A~%" total-flow)))

;;;; Idea is the following: we only consider the "useful" rooms, that
;;;; is, having a valve with a positive flow.
;;;; We see this as a weighted graph, the weight on each edge being the
;;;; actual distance between the rooms in the original graph.
;;;; Now, we simple do a DFS on this graph, with the following adaptations:
;;;; - we keep track of the rooms seen on the path, so as to never loop
;;;; - we also keep track of the time remaining, and we stop whenever we
;;;; reach the maximal allowed time
;;;; - we keep track of the current "score", given by the total flow

;;; Each VERTEX is a list of length 4:
;;; - The room, given as the "useful valve number"
;;; - The time remaining *after* we opened the valve here
;;; - A list of the valves that are already opened
;;; - The current "score"
(defun next-useful-valves (vertex)
  (destructuring-bind (v rem seen score) vertex
    (loop :for u :below (length *valves/useful*)
          :for dist = (distance/useful v u)
          :when (and (< dist rem)
                     (not (member u seen :test #'eq)))
            :collect (let ((u-opened-at (- rem dist 1)))
                       (cons (list u
                                   u-opened-at
                                   (cons u seen)
                                   (+ score (* u-opened-at (valve-flow/useful u))))
                             dist)))))

;;; We simply do the DFS as explained before
;;; We need to do it once for each possible useful valve, as the starting
;;; point (room AA) is not a useful one.
;;; Another solution would have been to add it to the useful rooms
(defun make-initial-vertex (initial-time initial-vertex i)
  (let* ((v  (aref *valves/useful* i))
         (dist (distance initial-vertex v))
         (rem (- initial-time dist 1))
         (score (* rem (valve-flow/useful i))))
    (list i rem
          (list i)
          score)))

(defun solve (initial-vertex initial-time)
  (loop :with max-score = 0
        :with path = nil
        :for i :from 0
        :for v :across *valves/useful*
        :do (dfs #'next-useful-valves
                 (make-initial-vertex initial-time initial-vertex i)
                 :at-vertex (lambda (v parent cost visited)
                              (declare (ignorable cost visited))
                              (let ((new-score (nth 3 v)))
                                (when (< max-score new-score)
                                  (setf max-score new-score)
                                  (setf path (cons (car v)
                                                   (nth 2 parent)))))))
        :finally
           (let ((sol (reverse (mapcar 'useful-valve->valve path))))
             ;; (replay-sol sol)
             (return (values max-score sol)))))


(defparameter *prune-function* (constantly nil))

;;;; For the elephant, we simply "duplicate" the state
;;;; We keep two "timers", one for the human and one for the elephant
;;;; We generate all the possible moves the following way:
;;;; The human picks a next room, and opens it
;;;; The elephant picks anther room, and opens it
;;;; CURRENT POSSIBLE BUG: if one has no time, other won't move ?
(defun next/elephant (u u-opened-at v-ele rem-ele seen score)
  "U is the vertex on which the human landed
U-OPENED-AT is the time remaining after valve U has been opened
V-ELE is the current elephant location
REM-ELE is the remaining time \"for the elephant\"
SEEN is the list of valves already opened
SCORE is the current score"
  (loop :for u-ele :below (length *valves/useful*)
        :for dist-ele = (distance/useful v-ele u-ele)
        :when (and (< dist-ele rem-ele)
                   (not (member u-ele seen))
                   (not (funcall *prune-function*)))
          :collect (let ((u-ele-opened-at (- rem-ele dist-ele 1)))
                     (cons (list u
                                 u-opened-at
                                 u-ele
                                 u-ele-opened-at
                                 (cons u-ele seen)
                                 (+ score (* u-ele-opened-at
                                             (valve-flow/useful u-ele))))
                           nil))))

(defun next-useful-valves-elephant (vertex)
  (destructuring-bind (v rem v-ele rem-ele seen score) vertex
    (loop :for u :below (length *valves/useful*)
          :for dist = (distance/useful v u)
          :when (and (< dist rem)
                     (not (member u seen :test #'eq)))
            :nconc (let ((u-opened-at (- rem dist 1)))
                     (incf score (* u-opened-at (valve-flow/useful u)))
                     (next/elephant u u-opened-at v-ele rem-ele (cons u seen) score)))))

(defun make-initial-vertex-elephant (initial-time initial-vertex i j)
  (let* ((v  (aref *valves/useful* i))
         (v-ele (aref *valves/useful* j))
         (dist (distance initial-vertex v))
         (dist-ele (distance initial-vertex v-ele))
         (rem (- initial-time dist 1))
         (rem-ele (- initial-time dist-ele 1))
         (score (* rem (valve-flow/useful i)))
         (score-ele (* rem-ele (valve-flow/useful j))))
    (list i rem
          j rem-ele
          (list j i)
          (+ score score-ele))))

(defun solve-elephant (initial-vertex initial-time)
  (let ((max-score 0)
        (n (length *valves/useful*)))
    (dotimes-product ((i n) (j n))
      (when (< i j)
        (format t "Starting with human at ~2D, elephant at ~2D~%" i j)
        (dfs #'next-useful-valves-elephant
             (make-initial-vertex-elephant initial-time initial-vertex i j)
             :at-vertex (lambda (v parent cost visited)
                          (declare (ignorable parent cost visited))
                          (let ((new-score (nth 5 v)))
                            (when (< max-score new-score)
                              (setf max-score new-score)))))))
    max-score))

(defun answer-ex-16-1 ()
  (init "../inputs/input16")
  (solve *AA* 30))

(defun answer-ex-16-2 ()
  (init "../inputs/input16")
  (solve-elephant *AA* 30))
