(in-package #:aoc2024/ex13)

(defparameter *input* #P"input")
(defparameter *test* #P"test")

(defun parse-game-rules (file)
  (read-file-as-lines-blocks file
                             :parse 'collect-integers-in-line
                             :parse-block (lambda (b)
                                            (destructuring-bind ((a b)
                                                                 (c d)
                                                                 (x y))
                                                b
                                              (list `((,a ,c)
                                                      (,b ,d))
                                                    `(,x ,y))))))

;;; After a push, we are at positions
;;; M * P
;;; where M is the "machine matrix"

(defun det-2x2 (matrix)
  (destructuring-bind ((a b)
                       (c d))
      matrix
    (- (* a d) (* b c))))

(defun invert-matrix (matrix)
  (destructuring-bind ((a b)
                       (c d))
      matrix
    (let ((det (det-2x2 matrix)))
      (unless (zerop det)
        `((,(/ d det)     ,(/ (- b) det))
          (,(/ (- c) det) ,(/ a det)))))))

(defun tokens-spent (machine goal &optional (bound 100))
  (let ((inverse (invert-matrix machine)))
    (if inverse
        (destructuring-bind (a b)
            (matrix*vector inverse goal)
          (if (and (integerp a)
                   (integerp b)
                   (or (not bound) (<= a bound))
                   (or (not bound) (<= b bound)))
              (+ (* 3 a) b)
              0))
        0)))

(defun matrix*vector (matrix vector)
  (loop :for row :in matrix
        :collect (loop :for m :in row
                       :for v :in vector
                       :sum (* m v))))

(defun answer-ex-13-1 (file)
  (loop :with machines = (parse-game-rules file)
        :for (machine goal) :in machines
        :sum (tokens-spent machine goal)))

(defun answer-ex-13-2 (file)
  (loop :with machines = (parse-game-rules file)
        :for (machine goal) :in machines
        :for corrected-goal = (mapcar (lambda (x)
                                        (+ 10000000000000 x))
                                      goal)
        :sum (tokens-spent machine corrected-goal nil)))
