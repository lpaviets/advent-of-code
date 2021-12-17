(in-package #:aoc2021/ex10)

(defparameter *pairs* (let ((table (make-hash-table)))
                        (dolist (pair'((#\( . #\))
                                       (#\[ . #\])
                                       (#\{ . #\})
                                       (#\< . #\>)))
                          (setf (gethash (car pair) table)
                                (cdr pair)))
                        table))

(defun error-points (char)
  (case char
    (#\) 3)
    (#\] 57)
    (#\} 1197)
    (#\> 25137)))

(defun read-sentence (line)
  (loop :with stack
        :with openings = '(#\( #\[ #\{ #\<)
        :for c :across line
        :if (member c openings)
          :do (push c stack)
        :else
          :do (let* ((top (car stack))
                     (matching (gethash top *pairs*)))
                (if (char= matching c)
                    (pop stack)
                    (return (error-points c))))
        :finally (return 0)))

;;; Part 2

(defun completion-points-char (char)
  (case char
    (#\( 1)
    (#\[ 2)
    (#\{ 3)
    (#\< 4)))

(defun completion-points-stack (stack)
  (loop :for pair :in stack
        :for pts = (completion-points-char pair)
        :for score = pts :then (+ (* 5 score)
                                 pts)
        :finally (return score)))

(defun completion-points-sentence (line)
  (loop :with stack
        :with openings = '(#\( #\[ #\{ #\<)
        :for c :across line
        :if (member c openings)
          :do (push c stack)
        :else
          :do (let* ((top (car stack))
                     (matching (gethash top *pairs*)))
                (if (char= matching c)
                    (pop stack)
                    (return)))
        :finally (return (and stack
                              (completion-points-stack stack)))))

(defun answer-ex-10-1 ()
  (let ((list (read-file-as-lines "../inputs/input10.txt")))
    (apply '+ (mapcar 'read-sentence list))))

(defun answer-ex-10-2 ()
  (let* ((list (read-file-as-lines "../inputs/input10.txt"
                                   ))
         (points (loop :for line :in list
                       :for pts = (completion-points-sentence line)
                       :when pts
                         :collect pts))
         (len (length points)))
    (nth (truncate len 2) (sort points '<))))
