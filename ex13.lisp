(in-package #:aoc2021/ex13)

(defun parse-points (list)
  (loop :for line :in list
        :collect (coma-separated-int-line line)))

(defun parse-folds (list)
  (loop :for fold :in list
        :collect (ppcre:register-groups-bind (axis ('parse-integer val))
                     ("fold along \(\\w\)=\(\\d+\)" fold)
                   (list (if (string= "x" axis) 0 1)
                         val))))

(defun split-points-folds (list)
  (let ((split (search '("") list :test 'equal)))
    (list (parse-points (subseq list 0 split))
          (parse-folds (subseq list (1+ split))))))

(defun fold-point (point axis val)
  (destructuring-bind (x y) point
    (if (zerop axis)
        (list (min x (- val (- x val))) y)
        (list x (min y (- val (- y val)))))))

(defun fold (list axis val)
  (remove-duplicates
   (mapcar (lambda (point) (fold-point point axis val)) list)
   :test 'equal))

;;; Part 2
(defun array-from-points (points)
  (destructuring-bind (height width)
      (loop :for (x y) :in points
            :maximize x :into width
            :maximize y :into height
            :finally (return (list (1+ height) (1+ width))))
    (let ((array (make-array (list height width))))
      (loop :for (x y) :in points :do
        (setf (aref array y x) 1))
      (loop :for j :below height :do
            (loop :for i :below width :do
              (format t "~a " (if (zerop (aref array j i)) " " "*")))
            (terpri)))))

(defun answer-ex-13-1 ()
  (let ((list (read-file-as-lines "inputs/input13.txt")))
    (destructuring-bind (points folds) (split-points-folds list)
      (destructuring-bind (axis val) (car folds)
        (length (fold points axis val))))))

(defun answer-ex-13-2 ()
  (let ((list (read-file-as-lines "inputs/input13.txt")))
    (destructuring-bind (points folds) (split-points-folds list)
      (dolist (fold folds)
        (setf points (fold points (car fold) (cadr fold))))
      (array-from-points points))))
