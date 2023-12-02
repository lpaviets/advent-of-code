(in-package #:aoc2021/ex24)

(declaim (optimize (debug 3) (speed 0)))

(defparameter *memory* (make-array 4))
(defparameter *variables* '((w . 0)
                            (x . 1)
                            (y . 2)
                            (z . 3)))

(defparameter *functions* '((add . +)
                            (mul . *)
                            (div . truncate)
                            (mod . mod)
                            (eql . eql-int)))

(defun eql-int (x y)
  (if (= x y) 1 0))

(defun read-instr (line)
  (loop :for start = 0 :then new
        :for (elt new) = (multiple-value-list
                          (read-from-string line nil nil :start start))
        :while elt
        :collect elt))

(defun execute-instr (instr &optional inputs)
  (if (eq (car instr) 'inp)
      (setf (aref *memory*
                  (cdr (assoc (second instr) *variables*)))
            (pop inputs))
      (let ((fun (cdr (assoc (car instr) *functions*)))
            (memcell (cdr (assoc (second instr) *variables*)))
            (operand (let ((val (third instr)))
                       (if (integerp val)
                           val
                           (let ((cell (cdr (assoc (third instr) *variables*))))
                             (aref *memory* cell))))))
        (setf (aref *memory* memcell)
              (funcall fun (aref *memory* memcell) operand)))))

;;; WARNING: DOES NOT WORK:
;;; 'inputs' (obviously) does not change, so we always pop the same value
;;; if there are several inputs (this works fine for a single inut though)
;;; However, we do not need it for the exercise, so this might be fixed
;;; later

(defun execute-prog (instrs &optional inputs)
  (setf *memory* (make-array 4))
  (loop :for instr :in instrs
        :do (execute-instr instr inputs)
        :finally (return *memory*)))

;; Analysis of the input: it is made of 14 blocks of instructions, each one
;; starting by a 'inp w'.
;; Those blocks differ only at two places, where x and y are respectively
;; added a constant, which differs on each block. Let v1 and v2 be those
;; constants. The first block of my input is:
;;
;; inp w
;; mul x 0                                 ; Reset x to 0
;; add x z                                 ; x = z
;; mod x 26                                ; x = z mod 26
;; div z 1                                 ; No-op or z = z / 26 depending on v1
;; add x 10                                ; x = z % 26 + v1
;; eql x w                                 ; x = 1 if (input = z % 26 + v1) else 0
;; eql x 0                                 ; x = 1 if (input != z % 26 + v1) else 0
;; mul y 0                                 ; Reset y
;; add y 25                                ; y = 25
;; mul y x                                 ; y = 25 if (input != z % 26 + v1) else 0
;; add y 1                                 ; y = 26 ... else 1
;; mul z y                                 ; z = 26*z' if ... else z'
;; mul y 0                                 ; Reset y
;; add y w                                 ; y = input
;; add y 12                                ; y = input + v2
;; mul y x                                 ; y = input + v2 if ... else 0
;; add z y                                 ; z = 26z' + input + v2 if (input != z%26 + v1) else z'

;; Then, assuming the memory is [w1 x1 y1 z1] before each block, and
;; [w2 x2 y2 z2] after, we have:
;; - w2 = input
;; - x2 irrelevant and will be reset to 0 in the next block
;; - y2 irrelevant and will be reset to 0 in the next block
;; - z2 = 26 * z1 + input + v2 if (input != z1 % 26 + v1) else 26*z1

;; Moreover:
;; - v1 > 0 => v1 > 9. This means that if v1 is positive, then the check
;; "input == ..." cannot be met, as input <= 9
;; - When v1 > 0, z1 is always divided by 1 after that; when v1 < 0, z1 is
;; always divided by 26
;; - z is always multiplied by 26 + added something (0 or a combination of
;; input and v2), or divided by 26

;; Hence, z can be seen a number in base26, that is shifted right or left in
;; each block, and added a value depending on input. With this in mind:
;; - If v1 > 0:
;;   - z is shifted left, and added input + v2
;; - Else:
;;   - z is shifted right, and let z0 be the popped digit (former last)
;;   - If z0 + v1 == input, do nothing
;;   - Else, shift z left and set its last digit to input + v2

;; At the end, we win if z = 0, that is, the 'stack' is empty
;; There are 7 times where v1 > 0 (and so 7 where v1 < 0) in my input
;; Hence, to have an empty stack, because we necessarily shift left 7 times
;; (each time v1 > 0), we must have each 'v1 < 0' do nothing.
;; This is the same as requiring that every time, input = z0 + v1

(defun get-v1-v2 (instrs)
  (loop :with remaining = instrs
        :with v1
        :with v2
        :while remaining :do
          (setf remaining (nthcdr 5 remaining))
          (setf v1 (third (car remaining)))
          (setf remaining (nthcdr 10 remaining))
          (setf v2 (third (car remaining)))
          (setf remaining (nthcdr 3 remaining))
        :collect (list v1 v2)))

(defun get-conditions (vals)
  (loop :with to-pop
        :for i :from 0
        :for (v1 v2) :in vals
        :if (plusp v1) :do
          (push (cons i v2) to-pop)
        :else
          :collect
          (let ((popped (pop to-pop)))
            (list i (car popped) (+ (cdr popped)
                                    v1)))))

(defun array-to-int (array)
  (loop :for val = 0 :then (+ (* val 10)
                              digit)
        :for digit :across array
        :finally (return val)))

(defun largest-possible (conditions)
  (let ((sol (make-array 14 :initial-element 9)))
    (loop :for (d1 d2 delta) :in conditions
          :if (<= delta 0) :do
            (setf (aref sol d2) 9
                  (aref sol d1) (+ 9 delta))
          :else :do
            (setf (aref sol d1) 9
                  (aref sol d2) (- 9 delta)))
    sol))

(defun smallest-possible (conditions)
  (let ((sol (make-array 14 :initial-element 1)))
    (loop :for (d1 d2 delta) :in conditions
          :if (>= delta 0) :do
            (setf (aref sol d2) 1
                  (aref sol d1) (+ 1 delta))
          :else :do
            (setf (aref sol d1) 1
                  (aref sol d2) (- 1 delta)))
    sol))

(defun answer-ex-24-1 ()
  (let* ((program (read-file-as-lines "../inputs/input24.txt"
                                      :parse 'read-instr))
         (conditions (get-conditions (get-v1-v2 program)))
         (largest (largest-possible conditions)))
    (array-to-int largest)))

(defun answer-ex-24-2 ()
    (let* ((program (read-file-as-lines "../inputs/input24.txt"
                                      :parse 'read-instr))
         (conditions (get-conditions (get-v1-v2 program)))
         (smallest (smallest-possible conditions)))
    (array-to-int smallest)))
