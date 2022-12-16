(in-package #:aoc2022/ex16)

(defparameter *valves* nil)
(defparameter *shortest-paths* nil)
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

(defun valve-neighbours (valve)
  (second (gethash valve *valves*)))

(defun distance (from to)
  (aref *shortest-paths* from to))



(defun build-shortest-paths ()
  (setf *shortest-paths* nil)
  (let* ((n (hash-table-count *valves*))
         (matrix (make-array (list n n) :initial-element nil)))
    (loop :for valve :being :the :hash-keys :of *valves*
          :for i :from 0
          :do (dolist (next (valve-neighbours valve))
                (setf (aref matrix valve next) 1)))
    (setf *shortest-paths* (shortest-path-all-to-all matrix))))

(defun non-zero-valves ()
  (let ((useful-valves (loop :for k :being :the :hash-key :of *valves*
                               :using (:hash-value v)
                             :when (plusp (car v))
                               :collect k)))
    (sort useful-valves #'> :key #'valve-flow)))

(defun valves-maximal-flow (time)
  (let ((opened (ceiling time 2))
        (valves (non-zero-valves)))
    (loop :for valve :in valves
          :for i :below opened
          :sum (valve-flow valve))))

(defun init (file)
  (parse-input file)
  (build-shortest-paths))

(defun prune-p (score remaining-time current-best max-flow)
  (< (* remaining-time max-flow)
     (- current-best score)))

;;;; Naive backtracking (decrease 1 minute per valve opening/walk to next room,
;;;; and maximize over every possible choice) is too slow: we end up walking aimlessly
;;;; Idea: wherever we are, the best thing to do is either to open the valve, or to
;;;; walk to a not-yet-opened useful valve, using the shortest possible path.
(defun highest-flow ()
  (let ((max-flow (valves-maximal-flow 30))
        (pruned 0))
    ;; AUX returns the best flow attainable given that:
    ;; - We are standing in the room POS
    ;; - REMAINING-TIME minutes remain
    ;; - USEFUL-VALVES are the currently-closed (non-zero) valves
    ;; - FLOW-INC is the flow currently coming from the opened valves
    (labels ((aux (flow             ; current flow
                   remaining-time   ; time remaining
                   useful-valves    ; remaining useful valves to go to
                   flow-inc         ; current flow from opened valves
                   pos              ; current position
                   current-solution ; current valve opening order
                   current-best)    ; current best flow, used to prune
               (cond
                 ((= remaining-time 0)
                  (values flow current-solution))
                 ((prune-p flow remaining-time current-best max-flow)
                  (incf pruned)
                  (values 0 nil))
                 ((null useful-valves)
                  (values (+ flow (* flow-inc remaining-time)) current-solution))
                 (t
                  (multiple-value-bind (best-with-move solution)
                      (loop :with current-max = current-best
                            :with current-max-solution = current-solution
                            :for next :in useful-valves
                            :for distance = (min remaining-time (distance pos next))
                            :for (next-val solution)
                              = (multiple-value-list
                                 (aux (+ flow (* distance flow-inc))
                                      (- remaining-time distance)
                                      useful-valves
                                      flow-inc
                                      next
                                      current-solution
                                      current-max))
                            :when (< current-max next-val)
                              :do (setf current-max next-val
                                        current-max-solution solution)
                            :finally (return (values current-max
                                                     current-max-solution)))
                    (if (not (member pos useful-valves))
                        (values best-with-move solution)
                        (multiple-value-bind (best-aux best-aux-solution)
                            (aux (+ flow flow-inc)
                                 (1- remaining-time)
                                 (remove pos useful-valves)
                                 (+ flow-inc (valve-flow pos))
                                 pos
                                 (cons pos current-solution)
                                 (max best-with-move current-best))
                          (if (< best-with-move best-aux)
                              (values best-aux best-aux-solution)
                              (values best-with-move solution)))))))))
      (multiple-value-bind (flow sol) (aux 0 30 (non-zero-valves) 0 *AA* nil -1)
        ;; (replay-sol (reverse sol))
        ;; (format t "~&Total branches pruned: ~D~%" pruned)
        (values flow (reverse sol))))))

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

(defun test ()
  (init "test")
  (highest-flow))

(defun answer-ex-16-1 ()
  (init "../inputs/input16")
  (highest-flow))

(defun answer-ex-16-2 ())
