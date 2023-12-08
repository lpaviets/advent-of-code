(in-package #:aoc2023/ex8)

(defparameter *current* nil)
(defparameter *moves* nil)
(defparameter *graph* nil)

(defparameter *finished-p* nil)


(defun read-node (line)
  (ppcre:register-groups-bind ((#'read-from-string node left right))
      ("^(\\w+) = \\((\\w+), (\\w+)\\)$" line)
    (list node (cons left right))))

(defun parse-file (file)
  (destructuring-bind ((moves) graph)
      (read-file-as-lines-blocks file)
    (setf moves (coerce moves 'list))
    (setf (cdr (last moves)) moves)     ; circular movements
    (list moves (mapcar 'read-node graph))))

(defun initialize-structures (moves graph init)
  (setf *current* (coerce init 'vector))
  (setf *finished-p* nil)
  (setf *moves* moves)
  (setf *graph* (make-hash-table :size (length graph) :test 'eq))
  (dolist (node graph)
    (setf (gethash (first node) *graph*) (second node))))

(defun node-start-p (node)
  (char= (char (symbol-name node) 2) #\A))

(defun node-end-p (node)
  (char= (char (symbol-name node) 2) #\Z))

(defun make-move (dir endp)
  (setf *finished-p* t)
  (loop :with dir-fun = (ecase dir
                          (#\L 'car)
                          (#\R 'cdr))
        :for i :below (length *current*)
        :for current = (aref *current* i)
        :for next = (funcall dir-fun (gethash current *graph*))
        :for next-end-p = (funcall endp next)
        :unless next-end-p
          :do (setf *finished-p* nil)
        :collect (setf (aref *current* i) next)))

(defun answer-ex-8-1 ()
  (destructuring-bind (moves graph)
      (parse-file "../inputs/input8.txt")
    (initialize-structures moves graph (list 'aaa)))
  (loop :for dir :in *moves*
        :for i :from 0
        :thereis (and *finished-p* i)
        :do (make-move dir (lambda (node) (eq node 'zzz)))))

(defun find-period (start)
  (setf *finished-p* nil)
  (let ((*current* (vector start)))
    (loop :for dir :in *moves*
          :for i :from 0
          :thereis (and *finished-p* i)
          :do (make-move dir 'node-end-p))))

(defun answer-ex-8-2 ()
  (destructuring-bind (moves graph)
      (parse-file "../inputs/input8.txt")
    ;; (parse-file "test.txt")
    (initialize-structures moves graph (remove-if-not 'node-start-p
                                                      (mapcar 'car graph))))
  ;; We remark that there is a (actual !) period:
  ;; for every starting node n_i, there is a period
  ;; p_i such that at step k*p_i the i-th "node" is on an
  ;; ending node, for every k.
  ;;
  ;; We simply find this period for every node and compute their LCM
  (let* ((n (length *current*))
         (periods (make-array n :initial-element 0)))
    (dotimes (i n)
      (setf (aref periods i) (find-period (aref *current* i))))
    (reduce 'lcm periods)))
