(in-package #:aoc2023/ex3)

(defclass anchored-number ()
  ((val :initarg :val :accessor val)
   (line :initarg :line :accessor line)
   (col-start :initarg :start :accessor col-start)
   (col-end :initarg :end :accessor col-end)))

;; Part 1

;; Many other solutions. One could read the whole file as a single
;; line (containing newlines), and find the position of numbers in
;; that line, then convert them back to array positions.
(defun get-number-at-pos (grid i j)
  "Return the next number in GRID, starting from position (I, J)"
  (let* ((w (array-dimension grid 1))
         (h (array-dimension grid 0)))
    (cond
      ((>= j (1- w))
       (if (>= i (1- h))
           nil
           (get-number-at-pos grid (1+ i) 0)))
      ((digit-char-p (aref grid i j))
       (loop :with res = (digit-char-p (aref grid i j))
             :for new-j :from (1+ j) :below w
             :for new-digit = (digit-char-p (aref grid i new-j))
             :while new-digit
             :do (setf res (+ new-digit (* 10 res)))
             :finally (return (values (make-instance 'anchored-number :val res
                                                                      :line i
                                                                      :start j
                                                                      :end (1- new-j))
                                      (list i new-j)))))
      (t (loop :for new-j :from j :below w
               :until (digit-char-p (aref grid i new-j))
               :finally (return (get-number-at-pos grid i new-j)))))))

(defun get-all-numbers (grid)
  (loop :for i = 0 :then (first pos)
        :for j = 0 :then (second pos)
        :for (num pos) = (multiple-value-list (get-number-at-pos grid i j))
        :while num
        :collect num))

(defun grid-symbol-p (grid i j)
  (let ((x (aref grid i j)))
    (and (not (digit-char-p x))
         (not (char= x #\.)))))

(defun number-valid-p (grid num)
  (loop :for j :from (col-start num) :to (col-end num)
        :for neighbours = (neighbours (line num) j grid :diagonal t)
        :thereis (loop :for (ni nj) :in neighbours
                       :thereis (grid-symbol-p grid ni nj))))

(defvar *gears* (make-hash-table :test 'equal))
(defun collect-maybe-gears (grid)
  (do-array (i j x grid)
    (when (char= x #\*)
      (setf (gethash (list i j) *gears*) (cons 0 1)))))

(defun number-adjacent-p (i j num)
  (let ((ni (line num))
        (start (col-start num))
        (end (col-end num)))
    (or (and (= ni i) (or (= start (1+ j))  ; same line, to the left
                          (= end (1- j))))  ; same line, to the right
        (and (or (= ni (1- i))              ; line below
                 (= ni (1+ i)))             ; line above
             (<= (1- start) j (1+ end)))))) ; in the bounds

(defun gear-p (i j nums)
  (loop :with adjacent = 0
        :with res = 1
        :for num :in nums
        :while (<= adjacent 2)
        :when (number-adjacent-p i j num)
          :do (incf adjacent)
              (setf res (* res (val num)))
        :finally (if (= adjacent 2)
                     (return res)
                     (return 0))))

(defun answer-ex-3-1 ()
  (let ((grid (read-file-as-array "../inputs/input3.txt")))
    (loop :for num :in (get-all-numbers grid)
          :when (number-valid-p grid num)
            :sum (val num))))

(defun answer-ex-3-2 ()
  (let* ((grid (read-file-as-array "../inputs/input3.txt"))
         (nums (get-all-numbers grid))
         (acc 0))
    (do-array (i j x grid)
      (when (char= x #\*)
        (incf acc (gear-p i j nums))))
    acc))
