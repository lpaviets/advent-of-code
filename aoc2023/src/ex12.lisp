(in-package #:aoc2023/ex12)

(defparameter *cache* nil)

(defun damaged-p (line pos)
  (char= (char line pos) #\#))

(defun operational-p (line pos)
  (char= (char line pos) #\.))

(defun parse-line (line)
  (destructuring-bind (lava groups)
      (ppcre:split " " line)
    (list lava (collect-integers-in-line groups))))

(defun count-possible-matches (line records)
  (setf *cache* (make-hash-table :test 'equalp))
  (labels ((aux (index records in-block)
             (or (gethash (list index records in-block) *cache*)
                 (setf (gethash (list index records in-block) *cache*)
                       (let ((current-record (car records))
                             (other-records (cdr records))
                             (length (length line)))
                         (cond
                           ((endp records) ; no damaged left
                            (if (find #\# line :test 'char= :start index)
                                0
                                1))
                           ((and (= index (1- length)) in-block)
                            ;; End of the line while filling a block
                            (cond
                              (other-records ; more than 1 block to place
                               0)
                              ((< 1 current-record) ; more than 1 damaged to place
                               0)
                              ((and (= 1 current-record) (not (operational-p line index)))
                               1)
                              ((and (= 0 current-record) (not (damaged-p line index)))
                               1)
                              (t 0)))
                           ((and (= index (1- length)) (not in-block))
                            ;; End of the line, not in a block
                            (cond
                              (other-records ; more than 1 block to place
                               0)
                              ((and (= 1 current-record) (not (operational-p line index)))
                               1)
                              (t 0)))
                           ((and in-block (= current-record 0)) ; end of a block
                            (if (damaged-p line index)
                                0
                                (aux (1+ index) other-records nil)))
                           (in-block    ; middle of a block
                            (if (operational-p line index)
                                0
                                (aux (1+ index) (cons (1- current-record) other-records) t)))
                           ((not in-block)
                            (cond
                              ((operational-p line index)
                               (aux (1+ index) records nil))
                              ((damaged-p line index)
                               (aux (1+ index) (cons (1- current-record) other-records) t))
                              (t
                               (+ (aux (1+ index) records nil)
                                  (aux (1+ index) (cons (1- current-record) other-records) t)))))))))))
    (aux 0 records nil)))

(defun %duplicate-line (line)
  (let* ((length (length line))
         (new-line (make-string (+ 4 (* 5 length)) :initial-element #\?)))
    (dotimes (i 5)
      (setf (subseq new-line (+ i (* i length)) (+ i (* (1+ i) length))) line))
    new-line))

(defun %duplicate-records (records)
  (loop :repeat 5
        :append records))

(defun duplicate-line-and-records (line records)
  (list (%duplicate-line line)
        (%duplicate-records records)))

(defun answer-ex-12-1 ()
  (loop :for (line records) :in (read-file-as-lines "../inputs/input12.txt"
                                                 :parse 'parse-line)
        :sum (count-possible-matches line records)))

(defun answer-ex-12-2 ()
  (loop :for (line records) :in (read-file-as-lines "../inputs/input12.txt"
                                                 :parse 'parse-line)
        :for (new-line new-records) = (duplicate-line-and-records line records)
        :sum (count-possible-matches new-line new-records)))
