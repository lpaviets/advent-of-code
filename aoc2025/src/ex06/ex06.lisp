(in-package #:aoc2025/ex6)

(defparameter *input* #P"input")
(defparameter *test* #P"test")

(defun read-operations (file)
  (destructuring-bind (op &rest lists)
      (nreverse (read-file-as-sexprs file))
    (cons op (nreverse lists))))

(defun compute-column (op &rest args)
  (reduce op args))

(defun read-operations-p2 (file)
  ;; Transposition
  (let* ((init (read-file-as-array file))
         (new  (make-array (reverse (array-dimensions init)))))
    (do-array (i j x init new)
      (setf (aref new j i) x))))

(defun convert-cephalopods-operations (operations)
  (loop :with (h w) = (array-dimensions operations)
        :with block = nil
        :with res = nil
        :for i :from 0 :below h
        :for op = (case (aref operations i (1- w))
                    (#\* '*)
                    (#\+ '+)
                    (t nil))
        ;; Make slice : i-th row, entire row minus the last char
        :for raw-row = (make-array (1- w)
                                   :displaced-to operations
                                   :displaced-index-offset (* i w))
        ;; Either nil (only blank space in row) or an integer
        :for row-num = (parse-integer (coerce raw-row 'string) :junk-allowed t)
        ;; Each block is (OP ARG1 ARG2 ...)
        ;; We stop adding args into the block when we encounter a blank line
        ;; We detect that by the fact the arg ROW-NUM is NIL
        ;; Each complete block gets pushed to another list
        :when op
          :do (push op block)
        :if row-num
          :do (push row-num block)
        :else
          :do (push (nreverse block) res)
              (setf block nil)
        :finally (push (nreverse block) res)
                 (return res)))

(defun answer-ex-6-1 (file)
  (let* ((operations (read-operations file))
         (all-columns (apply #'mapcar 'compute-column operations)))
    (reduce '+ all-columns)))

(defun answer-ex-6-2 (file)
  (let* ((operations (read-operations-p2 file))
         (cephalopod-ops (convert-cephalopods-operations operations)))
    (reduce '+ cephalopod-ops
            :key (lambda (args)
                   (reduce (car args) (cdr args))))))

