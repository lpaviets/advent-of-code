(in-package #:aoc2023/ex10)

(defparameter *pipes* '((#\| . (n s))
                        (#\- . (w e))
                        (#\L . (n e))
                        (#\J . (n w))
                        (#\7 . (w s))
                        (#\F . (e s))
                        (#\. . ())))
(defun reverse-dir (dir)
  (ecase dir
    (n 's)
    (s 'n)
    (e 'w)
    (w 'e)))

(defun pipe-dirs (pipe)
  (cdr (assoc pipe *pipes* :test 'char=)))

(defun can-move-pipe (start end dir)
  (and (member dir (pipe-dirs start))
       (member (reverse-dir dir) (pipe-dirs end))))

(defun can-move-to (pos grid)
  (destructuring-bind (x y) pos
    (loop :with start-pipe = (aref grid x y)
          :for (new-x new-y dir) :in (list (list (1+ x) y 's)
                                           (list (1- x) y 'n)
                                           (list x (1+ y) 'e)
                                           (list x (1- y) 'w))
          :when (array-in-bounds-p grid new-x new-y)
            :when (can-move-pipe start-pipe (aref grid new-x new-y) dir)
              :collect (list (list new-x new-y)))))

;; Modifies the grid, returns the source position
(defun find-source-and-pipe (grid)
  (do-array (i j x grid)
    (when (char= x #\S)
      (loop :with pos = (list i j)
            :for (pipe . next) :in *pipes*
            :do (setf (aref grid i j) pipe)
            :when (<= 2 (length (can-move-to pos grid)))
              :do (return-from find-source-and-pipe pos)))))


;; Part 2
;; Idea: "duplicate" the grid, adding pipes to keep the structure

(defun duplicate-grid (grid)
  (let ((source (find-source-and-pipe grid))
        (ext-grid (make-array (mapcar (lambda (x) (* 2 x)) (array-dimensions grid))
                              :initial-element #\.)))
    (do-array (i j x grid)
      (let ((ei (* 2 i))
            (ej (* 2 j)))
        (setf (aref ext-grid ei ej) x)
        (when (member x '(#\| #\7 #\F)) ; extend below
          (setf (aref ext-grid (1+ ei) ej) #\|))
        (when (member x '(#\- #\L #\F)) ; extend to the right
          (setf (aref ext-grid ei (1+ ej)) #\-))))
    ;; Now empty all the non-main-loop tiles
    (flet ((edges (pos)
             (can-move-to pos ext-grid)))
      (let ((distances (nth-value 1 (bfs #'edges (list (* 2 (car source))
                                                       (* 2 (cadr source)))
                                         :test 'equalp))))
        (do-array (i j x ext-grid ext-grid)
          (unless (gethash (list i j) distances)
            (setf (aref ext-grid i j) #\.)))))))

(defun on-grid-border-p (pos grid)
  (destructuring-bind (x y) pos
    (destructuring-bind (h w) (array-dimensions grid)
      (or (zerop x)
          (zerop y)
          (= x (1- h))
          (= y (1- w))))))

(defun connected-component-size (component grid)
  (loop :with outsidep = nil
        :for pos :being :the :hash-keys :of component
        :for (x y) = pos
        :when (on-grid-border-p pos grid)
          :do (return 0)
        :count (and (evenp x) (evenp y))))

(defun count-all-enclosed (grid)
  (let ((unexplored (make-hash-table :test 'equalp))
        (edges (lambda (pos)
                 (loop :for nghb :in (neighbours (car pos) (cadr pos) grid)
                       :when (char= #\. (apply 'aref grid nghb))
                         :collect (list nghb)))))
    (do-array (i j x grid)
      (when (char= x #\.)
        (setf (gethash (list i j) unexplored) t)))
    (loop :for component :in (connected-components unexplored edges :test 'equalp)
          :sum (connected-component-size component grid))))

(defun answer-ex-10-1 ()
  (let* ((grid (read-file-as-array "../inputs/input10.txt"))
         (source (find-source-and-pipe grid)))
    (flet ((edges (pos)
             (can-move-to pos grid)))
      (let ((distances (nth-value 1 (bfs #'edges source :test 'equalp))))
        (argmax distances :key (lambda (node)
                                 (gethash node distances)))))))

(defun answer-ex-10-2 ()
  (let* ((grid (read-file-as-array "../inputs/input10.txt")))
    (count-all-enclosed (duplicate-grid grid))))
