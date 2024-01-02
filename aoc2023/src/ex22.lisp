(in-package #:aoc2023/ex22)

(defun parse-brick (line)
  (let ((parsed (mapcar 'collect-integers-in-line (ppcre:split "~" line))))
    (sort parsed 'lexicographic<)))

(defun parse-file (file)
  (let* ((bricks (read-file-as-lines file :parse 'parse-brick))
         (sorted (sort bricks #'< :key #'caddar)) ; sort by start-Z
         (dims (loop :for ((xa ya za) (xb yb zb)) :in sorted
                     :maximize (max xa xb) :into x
                     :maximize (max ya yb) :into y
                     :maximize (max za zb) :into z
                     :finally (return (list x y z)))))
    (values sorted (make-array (mapcar '1+ dims)
                               :initial-element nil))))

(defun free-below-p (grid brick)
  (destructuring-bind ((xa ya za) (xb yb zb)) brick
    (cond
      ((= za 1) nil)
      ((< za zb) (not (aref grid xa ya (1- za))))
      (t (do-line* (b (list xa ya (1- za)) (list xb yb (1- zb)))
           (when (apply 'aref grid b)
             (return-from free-below-p nil)))
         t))))

(defun brick-fall (brick grid)
  (loop :with cur = (deepcopy brick)
        :while (free-below-p grid cur)
        :do (decf (third (first cur)))
            (decf (third (second cur)))
        :finally (return cur)))

(defun bricks-fall-all (bricks grid)
  (loop :for brick :in bricks
        :for i :from 0
        :for (start end) = (brick-fall brick grid)
        :do ;; (format t "~&Fall of ~A: moves to ~A~~~A~%" brick start end)
            (do-line* (b start end)
              (destructuring-bind (x y z) b
                (setf (aref grid x y z) i))))
  grid)

(defun find-brick (grid num)
  (labels ((convert (idx)
             (array-index-row-major grid idx)))
    (loop :for i :from 0 :below (array-total-size grid)
          :for val = (row-major-aref grid i)
          :when (and val (= num val))
            :minimize i :into min
            :and :maximize i :into max
          :finally (return (list (convert min)
                                 (convert max))))))

(defun show-result (grid n)
  (dotimes (i n)
    (let ((brick (find-brick grid i)))
      (format t "~&Brick ~A: position ~A~%" i brick))))

(defun supported-by (grid num)
  (let ((brick (find-brick grid num))
        res)
    (do-line* (b (first brick) (second brick))
      (destructuring-bind (x y z) b
        (if (= z 1)
            (return-from supported-by res)
            (let ((below (aref grid x y (1- z))))
              (when (and below (/= below num))
                (pushnew below res))))))
    res))

(defun supports-from-supported (supported)
  (let ((supports (make-hash-table)))
    (do-hash (k v supported)
      (dolist (s v)
        (push k (gethash s supports))))
    supports))

(defun all-supported (grid n)
  (ht-from-plist (loop :for i :from 0 :below n
                       :for supports = (supported-by grid i)
                       :collect i
                       :collect supports)))

(defun can-disintegrate (grid n)
  (let* ((supports (all-supported grid n))
         (disintegrate (deepcopy supports)))
    (do-hashvalues (sups supports)
      (when (and (not (endp sups))
                 (endp (cdr sups)))
        (remhash (car sups) disintegrate)))
    disintegrate))

(defun remove-change (brick supported supports)
  (let ((supported (deepcopy supported))
        (supports (deepcopy supports))
        (rem (list brick)))
    (loop :with count = 0
          :for new = (pop rem)
          :do (dolist (other (gethash new supports))
                (setf (gethash other supported)
                      (delete new (gethash other supported)))
                (unless (gethash other supported)
                  (push other rem)
                  (incf count)))
          :while rem
          :finally (return count))))

(defun answer-ex-22-1 ()
  (multiple-value-bind (bricks grid)
      (parse-file  "../inputs/input22.txt")
    (bricks-fall-all bricks grid)
    (hash-table-count (can-disintegrate grid (length bricks)))))

(defun answer-ex-22-2 ()
  (multiple-value-bind (bricks grid)
      (parse-file "../inputs/input22.txt")
    (bricks-fall-all bricks grid)
    (let* ((len (length bricks))
           (supported (all-supported grid len))
           (supports  (supports-from-supported supported)))
      (loop :for brick :from 0 :below len
            :sum (remove-change brick supported supports)))))
