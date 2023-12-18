(in-package #:aoc2023/ex18)

(defun parse-instruction (line)
  (destructuring-bind (dir n code)
      (ppcre:split " " line)
    (declare (ignore code))
    (list (case (char dir 0)
            (#\R :right)
            (#\L :left)
            (#\U :up)
            (#\D :down))
          (parse-integer n))))

(defun parse-hex-instruction (line)
  (destructuring-bind (dir n code)
      (ppcre:split " " line)
    (declare (ignore dir n))
    (list
     (ecase (char code 7)
       (#\0 :right)
       (#\1 :down)
       (#\2 :left)
       (#\3 :up))
     (parse-integer code :start 2 :end 7 :radix 16))))

;;; Part 2: too large to store every cell in the border
;;; What we do: shoelace formula + Pick's theorem !
;;; Unusual application ! For a family of (integral) points P,
;;; Shoelace => Area = S(P)
;;; Pick     => Area = i + b/2 - 1
;;; where b = nb of points of P on the border
;;;       i = _____             in the interior
;;; What we are really interested in here: i+b
;;; What we know: P, b
;;; => i+b = Area + 1 + b/2 = S(P) + 1 +b/2

;;; See the grids.lisp file of my personal utilities to see an
;;; implementation.
(defun border (instructions)
  (loop :for (dir n) :in instructions
        :for pos = '(0 0) :then next-pos
        :for next-pos = (grid-pos-in-direction pos dir n)
        :collect pos))

(defun answer-ex-18-1 ()
  (let* ((instructions (read-file-as-lines "../inputs/input18.txt"
                                           :parse 'parse-instruction))
         (border (border instructions)))
    (grid-area-lattice border)))

(defun answer-ex-18-2 ()
  (let* ((instructions (read-file-as-lines "../inputs/input18.txt"
                                           :parse 'parse-hex-instruction))
         (border (border-large instructions)))
    (grid-area-lattice border)))
