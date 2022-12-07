(in-package #:aoc2022/ex7)

(defclass aoc-file-or-dir ()
  ((parent :initarg :parent :reader parent)))

(defclass aoc-file (aoc-file-or-dir)
  ((name :initarg :name :reader name)
   (size :initarg :size :reader size)))

(defclass aoc-dir (aoc-file-or-dir)
  ((name :initarg :name :reader name)
   (files :initform (make-hash-table :test 'equal) :accessor files)
   (size :initform nil)))

;;;; Globals
(defparameter *root-dir* (make-instance 'aoc-dir
                                        :name "/"
                                        :parent nil))
(defparameter *current-dir* *root-dir*)

(defun reset ()
  (setf *root-dir* (make-instance 'aoc-dir
                                          :name "/"
                                        :parent nil))
  (setf *current-dir* *root-dir*))

;;;; File system utilities
(defun found-file (name)
  (gethash name (files *current-dir*)))

(defun add-file (name file)
  (setf (gethash name (files *current-dir*)) file))

(defun make-file-here (name size)
  (let ((found-file (found-file name)))
    (or found-file
        (let ((file (make-instance 'aoc-file
                                   :name name
                                   :size size
                                   :parent *current-dir*)))
          (add-file name file)))))

(defun make-dir-here (name)
  (let ((found-dir (found-file name)))
    (or found-dir
        (let ((dir (make-instance 'aoc-dir
                                  :name name
                                  :parent *current-dir*)))
          (add-file name dir)))))

(defmethod size ((dir aoc-dir))
  (or (slot-value dir 'size)
      (let ((total 0))
        (do-hashvalues (subfile (files dir))
          (incf total (size subfile)))
        (setf (slot-value dir 'size) total))))

(defun map-file-system (fun-dir &optional fun-file)
  (labels ((aux (file)
             (etypecase file
               (aoc-dir
                (do-hashvalues (subfile (files file))
                  (aux subfile))
                (when fun-dir (funcall fun-dir file)))
               (aoc-file
                (when fun-file (funcall fun-file file))))))
    (aux *root-dir*)))

;;;; Commands
(defun run-cd (dir)
  (setf *current-dir* (cond
                        ((equal dir "/") *root-dir*)
                        ((equal dir "..") (parent *current-dir*))
                        (t (found-file dir)))))

(defun run-ls (output)
  (loop :for (dir-or-size . name) :in output
        :if (eq dir-or-size 'dir)
          :do (make-dir-here name)
        :else
          :do (make-file-here name dir-or-size)))

(defun run-command (command)
  (ecase (first command)
    (cd (run-cd (second command)))
    (ls (run-ls (cdr command)))))

;;;; Parsing
(defun parse-ls-line (line)
  (if (string= (subseq line 0 3) "dir")
      (cons 'dir (subseq line 4))
      (let ((pos (position #\Space line)))
        (cons (parse-integer line :end pos)
              (subseq line (1+ pos))))))

(defun split-on-command (lines)
  (loop :for block = (loop :with command
                           :for (line . rest) :on lines
                           :for next = (car rest)
                           :do (cond
                                 ((string= "$ cd" (subseq line 0 4))
                                  (push 'cd command)
                                  (push (subseq line 5) command))
                                 ((string= "$ ls" line)
                                  (push 'ls command))
                                 (t
                                  (push (parse-ls-line line) command)))
                           :until (or (not next) (char= (char next 0) #\$))
                           :finally (setf lines rest)
                                    (return (nreverse command)))
        :while block
        :collect block))

(defun parse-input (file)
  (let* ((lines (read-file-as-lines file))
         (commands (split-on-command lines)))
    (dolist (cmd commands)
      (run-command cmd))))

;;; Questions:
(defun all-sizes-below (threshold)
  (let ((acc 0))
    (map-file-system (lambda (dir)
                       (when (< (size dir) threshold)
                         (incf acc (size dir)))))
    acc))

(defun missing-memory (wanted)
  (- wanted
     (- 70000000
        (size *root-dir*))))

(defun smallest-to-delete (to-free)
  (let ((min most-positive-fixnum))
    (map-file-system (lambda (dir)
                       (when (< to-free (size dir) min)
                         (setf min (size dir)))))
    min))

(defun answer-ex-7-1 ()
  (reset)
  (parse-input "../inputs/input7")
  (all-sizes-below 100000))

(defun answer-ex-7-2 ()
  (reset)
  (parse-input "../inputs/input7")
  (smallest-to-delete (missing-memory 30000000)))
