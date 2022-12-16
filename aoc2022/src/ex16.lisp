(in-package #:aoc2022/ex16)

(defparameter *valves* nil)
(defparameter *shortest-paths* nil)
;;;; TO FINISH: add a parameter sym -> number, to produce an adjacency matrix easily
;;;; Keep the hash-table representation too

(defun parse-line (line)
  (destructuring-bind (valve flow . neighbours)
      (ppcre:all-matches-as-strings "\\d+|[A-Z]{2}" line)
    (list (intern valve)
          (parse-integer flow)
          (mapcar #'intern neighbours))))

(defun parse-input (file)
  (let ((lines (read-file-as-lines file :parse #'parse-line)))
    (setf *valves* (ht-from-sequence lines
                                     :test 'eq
                                     :key #'car
                                     :value #'cdr))))

;;;; TO FINISH
;; (defun adjacency-matrix ()
;;   (let* ((length (length *valves*))
;;          (matrix (make-array (list length length) :initial-element nil)))
;;     (loop :for valve :in *valves*
;;           :for i :from 0
;;           :do (dolist (next (valve-neighbours ))))))

(defun valve-flow (valve)
  (first (gethash valve *valves*)))

(defun valve-neighbours (valve)
  (second (gethash valve *valves*)))

(defun valves-flow-all-opened ()
  (let ((total 0))
    (maphash (lambda (k v)
               (declare (ignore k))
               (incf total (car v)))
             *valves*)
    total))

(defun non-zero-valves ()
  (loop :for k :being :the :hash-key :of *valves* :using (:hash-value v)
        :when (plusp (car v))
          :collect k))

;;;; FLOYD WARSHALL
;;;; TO FINISH
(defun shortest-paths ()
  )

(defun prune-p (score remaining-time current-best max-flow)
  (< (* remaining-time max-flow) (- current-best score)))

;;;; Naive backtracking (decrease 1 minute per valve opening/walk to next room,
;;;; and maximize over every possible choice) is too slow and we end up walking aimlessly
;;;; Idea: wherever we are, the best thing to do is either to open the valve, or to
;;;; walk to a not-yet-opened useful valve in the shortest possible path.

;;;; TO FINISH: use floyd-warshall to shortcut things
(defun highest-flow ()
  (let ((max-flow (valves-flow-all-opened)))
    (labels ((aux (score           ; current score
                   remaining-time  ; time remaining
                   opened-valves   ; which valves have been opened
                   ;; useful-valves   ; remaining useful valves to go to
                   score-inc       ; current flow from opened valves
                   pos             ; current position
                   current-best)   ; current best score, used to prune
               (cond
                 ((= remaining-time 0)
                  (max score current-best))
                 ((prune-p score remaining-time current-best max-flow)
                  current-best)
                 ;; ((null useful-valves)
                 ;;  (max current-best (+ score (* score-inc remaining-time))))
                 (t
                  (let ((best-with-move
                          (loop :for current-max = current-best :then (max current-max next-val)
                                :for next :in (valve-neighbours pos)
                                :for next-val = (aux (+ score score-inc)
                                                     (1- remaining-time)
                                                     opened-valves
                                                     score-inc
                                                     next
                                                     current-max)
                                :maximize current-max)))
                    (if (or (zerop (valve-flow pos)) (member pos opened-valves))
                        best-with-move
                        (max best-with-move
                             (max (aux (+ score score-inc)
                                       (1- remaining-time)
                                       (cons pos opened-valves)
                                       (+ score-inc (valve-flow pos))
                                       pos
                                       (max best-with-move current-best))))))))))
      (aux 0 30 nil 0 'AA 0))))

(defun test ()
  (parse-input "test")
  (highest-flow))

(defun answer-ex-16-1 ())

(defun answer-ex-16-2 ())
