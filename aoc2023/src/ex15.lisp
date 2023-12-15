(in-package #:aoc2023/ex15)

(defun parse-file ()
  (let ((content (uiop:read-file-string "../inputs/input15.txt")))
    (ppcre:split "," (remove #\Newline content))))

(defun update-value (value char)
  (let ((code (char-code char)))        ; NON PORTABLE !
    (let* ((val (+ value code))
           (val (* 17 val))
           (val (rem val 256)))
      val)))

(defun hash (string)
  (reduce 'update-value string :initial-value 0))

(defun parse-op (op)
  (if (char= #\- (char op (1- (length op))))
      (list '- (subseq op 0 (- (length op) 1)))
      (let ((splitted (ppcre:split "=" op)))
        (list '= (first splitted) (parse-integer (second splitted))))))

(defun add-box (label val box)
  (let ((prev (assoc label box :test #'string=)))
    (if prev
        (setf (cdr prev) val)
        (push (cons label val) box)))
  box)

(defun remove-box (label box)
  (delete label box :key #'car :test #'string=))

(defun apply-op (op boxes)
  (let* ((op (parse-op op))
         (label (second op)))
    (symbol-macrolet ((box (aref boxes (hash label))))
      (if (eq (first op) '-)
          (setf box (remove-box label box))
          (setf box (add-box label (third op) box))))))

(defun value-box (box box-num)
  (loop :for i :from 1
        :for (lens-name . lens-val) :in (reverse box)
        :sum (* lens-val (1+ box-num) i)))

(defun answer-ex-15-1 ()
  (let ((initializations (parse-file)))
    (reduce '+ initializations :key 'hash)))

(defun answer-ex-15-2 ()
  (let ((initializations (parse-file))
        (boxes (make-array 256 :initial-element nil)))
    (dolist (op initializations)
      (apply-op op boxes))
    (loop :for i :from 0 :to 255
          :for box = (aref boxes i)
          :sum (value-box box i))))
