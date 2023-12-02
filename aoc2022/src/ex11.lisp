(in-package #:aoc2022/ex11)

(defparameter *monkeys* (make-array 8))
(defparameter *bored-monkeys* t)

(defclass monkey ()
  ((items :initarg :items :accessor items :type queue)
   (operation :initarg :operation :reader operation)
   (test :initarg :test :reader test :type fixnum)
   (output-true :initarg :true :reader output-true :type fixnum)
   (output-false :initarg :false :reader output-false :type fixnum)
   (inspected-items :initform 0 :accessor inspected-items :type fixnum)))

(declaim (inline monkey))
(defun monkey (num)
  (aref *monkeys* num))

;;;; "Clever" trick needed for part 2:
;;;; We are only ever interested in *where* the items go, not their actual
;;;; "worry level"
;;;; Their trajectory is determined by divisibility tests, different for each
;;;; monkey. However, those tests are not affected by the "worry level" MODULO
;;;; their divisor; if we take the product (actually, least common multiple is enough)
;;;; of the divisors, nothing changes, but the "worry levels" can be kept smaller.
(defparameter *worry-modulo* 0)
(defun set-worry-modulo ()
  (setf *worry-modulo* (reduce #'lcm *monkeys* :key #'test)))

;;;; Parsing the input
;;; Items: only read the items
;;; Test and its result: same
(defun read-line-keep-integers (line)
  (read-from-string (format nil "(~A)" (substitute-if-not #\Space #'digit-char-p line))))

;;; Operation and operands: read after the = sign
(defun parse-monkey-operation (line)
  (ppcre:register-groups-bind ((#'read-from-string op1) (#'intern fun) (#'read-from-string op2))
      ("= (.*) (.) (.*)$" line)
    (list fun op1 op2)))

;;; Parse a full monkey "block"
(defun parse-monkey (monkey)
  (destructuring-bind (num items operation test true false) monkey
    (declare (ignore num))
    (let ((items (read-line-keep-integers items))
          (operation (parse-monkey-operation operation))
          (test (car (read-line-keep-integers test)))
          (true (car (read-line-keep-integers true)))
          (false (car (read-line-keep-integers false))))
      (make-instance 'monkey
                     :items (apply #'make-queue items)
                     :operation operation
                     :test test
                     :true true
                     :false false))))

(defun parse-input (file)
  (map-into *monkeys* #'parse-monkey (read-file-as-lines-blocks file)))

;;;; Utilities
(defun add-item (monkey item)
  (queue-push (items monkey) item))

(defun update-worry (item op)
  (let ((res (apply (car op) (substitute item 'old (cdr op)))))
   (if *bored-monkeys*
       (truncate res 3)
       (mod res *worry-modulo*))))

(defun inspect-item (monkey)
  (let* ((item (queue-pop (items monkey)))    ; pick first item
         (worry (update-worry item (operation monkey)))) ; update worry
    (incf (inspected-items monkey)) ; Update inspected count
    ;; throw it to next monkey
    (if (zerop (mod worry (test monkey)))
        (add-item (monkey (output-true monkey)) worry)
        (add-item (monkey (output-false monkey)) worry))))

(defun monkey-turn (monkey)
  (loop :until (queue-empty-p (items monkey))
        :do (inspect-item monkey)))

(defun turn ()
  (map nil #'monkey-turn *monkeys*))

(defun monkeys-business ()
  (let ((sorted-monkeys (sort *monkeys* #'> :key #'inspected-items)))
    (* (inspected-items (elt sorted-monkeys 0))
       (inspected-items (elt sorted-monkeys 1)))))

(defun simulate-turns (n boredp)
  (let ((*bored-monkeys* boredp))
    (dotimes (_ n)
      (turn))))

;;;; Answers
(defun init ()
  (parse-input "../inputs/input11")
  (set-worry-modulo))

(defun answer-ex-11-1 ()
  (init)
  (simulate-turns 20 t)
  (monkeys-business))

(defun answer-ex-11-2 ()
  (init)
  (simulate-turns 10000 nil)
  (monkeys-business))
