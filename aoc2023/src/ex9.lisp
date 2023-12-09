(in-package #:aoc2023/ex9)

(defun differences (list)
  (mapcar '- (cdr list) list))

(defun extrapolate (list &key (backwards nil))
  (let (all-differences)
    (loop :for prev = list :then next
          :for next = (differences prev)
          :until (every 'zerop prev)
          :do (push (if backwards prev (reverse prev))
                    all-differences))
    (reduce (if backwards
                (lambda (x y) (- y x))
                '+)
            all-differences
            :key 'car)))

(defun extrapolate-backwards (list)
  (extrapolate list :backwards t))

(defun answer-ex-9-1 ()
  (let ((lines (read-file-as-lines "../inputs/input9.txt" :parse 'collect-integers-in-line)))
    (reduce '+ lines :key 'extrapolate)))

(defun answer-ex-9-2 ()
  (let ((lines (read-file-as-lines "../inputs/input9.txt" :parse 'collect-integers-in-line)))
    (reduce '+ lines :key 'extrapolate-backwards)))
