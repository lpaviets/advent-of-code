(in-package #:aoc2024/ex11)

(defparameter *input* #P"input")
(defparameter *test* #P"test")

(defun stone-list (file)
  (read-file-one-line file :parse 'collect-integers-in-line))

(defun digits-count (n)
  (loop :for current = n :then q
        :for (q r) = (multiple-value-list (truncate current 10))
        :count 1
        :while (plusp q)))

(defun split-digits (n &optional (digits (digits-count n)))
  (truncate n (expt 10 (/ digits 2))))

(defun evolve-step (stones)
  (let (new-stones digits)
    (dolist (stone stones new-stones)
      (cond
        ((zerop stone) (push 1 new-stones))
        ((evenp (setf digits (digits-count stone)))
         (multiple-value-bind (left right)
             (split-digits stone digits)
           (push left new-stones)
           (push right new-stones)))
        (t (push (* 2024 stone) new-stones))))))

(defun %count-stones-after (stone steps cache)
  (if (zerop steps)
      1
      (let ((data (cons stone steps)))
        (or (gethash data cache)
            (setf (gethash data cache)
                  (let ((digits (digits-count stone)))
                    (cond
                      ((zerop stone) (%count-stones-after 1 (1- steps) cache))
                      ((evenp digits)
                       (multiple-value-bind (left right)
                           (split-digits stone digits)
                         (+ (%count-stones-after left (1- steps) cache)
                            (%count-stones-after right (1- steps) cache))))
                      (t (%count-stones-after (* 2024 stone) (1- steps) cache)))))))))

(defun count-stones-after (stones steps)
  (let ((cache (make-hash-table :test 'equal)))
    (loop :for stone :in stones
          :sum (%count-stones-after stone steps cache))))

(defun answer-ex-11-1 (file)
  (let ((stones (stone-list file)))
    (dotimes (_ 25)
      (setf stones (evolve-step stones)))
    (length stones)))

(defun answer-ex-11-2 (file)
  (let ((stones (stone-list file)))
    (count-stones-after stones 75)))
