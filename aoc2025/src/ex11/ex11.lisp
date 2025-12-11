(in-package #:aoc2025/ex11)

(defparameter *input* #P"input")
(defparameter *test* #P"test")

(defun parse-rack (rack)
  (loop :for x :in (cl-ppcre:split ":? " rack)
        :collect (read-from-string x)))

(defun read-racks (file)
  (read-file-as-lines file :parse 'parse-rack))

(defun rack-to-edges (racks)
  (ht-from-sequence racks :test 'eq :key 'car :value 'cdr))

(defun count-paths-to-out (from racks)
  (let ((cache (make-hash-table :test 'eq))
        (edges (racks-to-edges racks)))
    (setf (gethash 'out cache) 1)
    (labels ((count-paths-aux (from)
               (or (gethash from cache)
                   (setf (gethash from cache)
                         (loop :for next :in (funcall edges from)
                               :sum (count-paths-aux next))))))
      (count-paths-aux from))))

(defun count-paths-to-out-passing-by (from racks others)
  (let ((cache (make-hash-table :test 'equal))
        (edges (racks-to-edges racks)))
    ;; Cache : keys are of the form (CURRENT REMAINING) where REMAINING is a
    ;; list of vertices to visit before exiting

    ;; Already visited everybody : 1 path out
    (setf (gethash (list 'out nil) cache) 1)
    ;; Still dudes to visit : no luck, no valid paths are possible still
    (loop :for k :from 1 :to (length others)
          :do (do-sequence-subsets (x (k others))
                (setf (gethash (list 'out (copy-list x)) cache) 0)))

    ;; Otherwise, simply update from time to time
    (labels ((count-paths-aux (from rem)
               (or (gethash (list from rem) cache)
                   (let ((rem-now (if (member from rem)
                                      (remove from rem)
                                      rem)))
                     (setf (gethash (list from rem-now) cache)
                           (loop :for next :in (funcall edges from)
                                 :sum (count-paths-aux next rem-now)))))))
      (values (count-paths-aux from others) cache))))

(defun answer-ex-11-1 (file)
  (let ((racks (read-racks file))
        (start 'you))
    (count-paths-to-out start racks)))

(defun answer-ex-11-2 (file)
  (let ((racks (read-racks file))
        (start 'svr)
        (passing-by (list 'dac 'fft)))
    (count-paths-to-out-passing-by start racks passing-by)))
