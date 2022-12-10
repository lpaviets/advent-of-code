(in-package #:aoc2022/ex10)

(defun parse-line (line)
  (let ((space (position #\Space line)))
    (multiple-value-bind (instr chars-read)
        (read-from-string line t nil :end space)
      (cons instr
            (or (parse-integer line :start chars-read :junk-allowed t) 0)))))

(defun signal-strength (step value)
  (* step value))

;;; Duplicate each 'ADDX' instruction into a NOOP + the original ADDX
;;; Then simulate using 1 step per instruction
(defun update-instructions (instructions)
  (loop :for instr :in instructions
        :if (eq (car instr) 'noop)
          :collect instr
        :else
          :collect (cons 'noop 0)
          :and collect instr))

(defun simulate-all-instructions (instructions)
  (loop :for (instr . val) :in (update-instructions instructions)
        :for step :from 1
        :for x = 1 :then (+ x val)
        :when (= 19 (mod step 40)) ; Sum *before* executing the instruction
                                   ; number 20, 60 ...
          :sum (* x (1+ step)))) ; But the multiplier is still 20, 60 ...

(defun lit-pixel-p (pixel sprite-pos)
  (<= (abs (- sprite-pos pixel)) 1))

(defun draw-instructions (instructions)
  (loop :with grid = (make-array (list 6 40) :initial-element #\.)
        :for step :from 1
        :for pixel = (mod step 40)
        :for (instr . val) :in (update-instructions instructions)
        :for x = 1 :then (+ x val)
        :when (lit-pixel-p pixel x)
          :do (setf (row-major-aref grid step) #\#)
        :finally (print-array grid)))

(defun answer-ex-10-1 ()
  (let ((instructions (read-file-as-lines "../inputs/input10" :parse 'parse-line)))
    (simulate-all-instructions instructions)))

(defun answer-ex-10-2 ()
  (let ((instructions (read-file-as-lines "../inputs/input10" :parse 'parse-line)))
    (draw-instructions instructions)))
