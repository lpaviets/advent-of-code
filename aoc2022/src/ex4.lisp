(in-package #:aoc2022/ex4)

(defun parse-line (line)
  (ppcre:register-groups-bind ((#'parse-integer a) (#'parse-integer b)
                               (#'parse-integer c) (#'parse-integer d))
      ("\(\\d+\)-\(\\d+\),\(\\d+\)-\(\\d+\)" line)
    (cons (cons a b) (cons c d))))

(defun point-in-range (pt range)
  (<= (car range) pt (cdr range)))

(defun range-in-range (r1 r2)
  (and (point-in-range (car r1) r2)
       (point-in-range (cdr r1) r2)))

(defun fully-contained-range (range-pair)
  (let ((r1 (car range-pair))
        (r2 (cdr range-pair)))
   (or (range-in-range r1 r2)
       (range-in-range r2 r1))))

(defun overlap-ranges (r1 r2) ;; Overlap <=> not disjoint
  (not (or (< (cdr r1) (car r2))
           (< (cdr r2) (car r1)))))

(defun answer-ex-4-1 ()
  (let ((ranges (read-file-as-lines "../inputs/input4" :parse #'parse-line)))
    (count-if #'fully-contained-range ranges)))

(defun answer-ex-4-2 ()
  (let ((ranges (read-file-as-lines "../inputs/input4" :parse #'parse-line)))
    (count-if (lambda (range-pair)
                (overlap-ranges (car range-pair) (cdr range-pair)))
              ranges)))

;;; Ugly loop-based solution:
(defun solve-day-4 ()
  (with-open-file (in "../inputs/input4")
    (loop :for line = (read-line in nil nil)
          :while line
          :for only-digit-line = (substitute-if-not #\Space #'digit-char-p line)
          :for (s1 e1 s2 e2) = (read-from-string (format nil "(~A)" only-digit-line))
          :count (or (<= s1 s2 e2 e1)
                     (<= s2 s1 e1 e2))
            :into contained
          :count (not (or (< e1 s2)
                          (< e2 s1)))
            :into overlap
          :finally (return (values contained overlap)))))
