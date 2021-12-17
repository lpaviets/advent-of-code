(in-package #:aoc2021/ex12)

(defparameter *graph* nil)
(defparameter *vertices* nil)
(defparameter *big-vertices* nil)

(defun add-vertex (str)
  (let ((vertex (read-from-string str))
        (bigp (every 'upper-case-p str)))
    (unless (assoc vertex *vertices*)
      (let ((idx (if *vertices*
                     (1+ (apply 'max (mapcar 'cdr *vertices*)))
                     0)))
        (push (cons vertex idx) *vertices*)
        (when bigp
          (push idx *big-vertices*))))
    vertex))

(defun vertices-and-edge-list (list)
  (loop :for line :in list
        :for (u v) = (ppcre:split "-" line)
        :collect (list (add-vertex u)
                       (add-vertex v))))

(defun make-graph (edges)
  (loop :initially (let ((size (length *vertices*)))
                     (setf *graph* (make-array (list size size))))
        :for (u v) :in edges
        :for i = (cdr (assoc u *vertices*))
        :for j = (cdr (assoc v *vertices*))
        :do (setf (aref *graph* i j) 1
                  (aref *graph* j i) 1)))

(defun init-graph (file)
  (setf *graph* nil
          *vertices* nil
          *big-vertices* nil)
  (let* ((list (read-file-as-lines file))
         (edges (vertices-and-edge-list list)))
    (make-graph edges)))

(defun format-path (visited)
  (format t "Path: ~{~a~^,~}~%" (loop :for idx :in (reverse visited)
                                      :for sym = (car (rassoc idx *vertices*))
                                      :for name = (symbol-name sym)
                                      :for bigp = (member idx *big-vertices*)
                                      :collect (if bigp name (string-downcase name)))))

;;; Part 2: add everything related to the "visit-small" and "start" arguments

(defun count-paths (visited pos end &optional visit-small start)
  (if (= pos end)
      (progn
        (format-path visited)
        1)
      (loop :for j :below (array-dimension *graph* 1)
            :for neighbourp = (plusp (aref *graph* pos j))
            :for bigp = (member j *big-vertices*)
            ;; Never visited the cave before
            :when (and neighbourp
                       (or bigp
                           (not (member j visited))))
              :sum (count-paths (cons j visited) j end visit-small start)
            ;; Small cave but still "joker" to visit it twice
            :when  (and neighbourp
                        (not bigp)
                        (member j visited)
                        visit-small
                        (/= j start))
              :sum (count-paths (cons j visited) j end nil start))))

(defun answer-ex-12-1 ()
  (init-graph "../inputs/input12.txt")
  (let ((start-idx (cdr (assoc 'start *vertices*)))
        (end-idx (cdr (assoc 'end *vertices*))))
    (count-paths (list start-idx) start-idx end-idx)))

(defun answer-ex-12-2 ()
  (init-graph "../inputs/input12.txt")
  (let ((start-idx (cdr (assoc 'start *vertices*)))
        (end-idx (cdr (assoc 'end *vertices*))))
    (count-paths (list start-idx) start-idx end-idx t start-idx)))
