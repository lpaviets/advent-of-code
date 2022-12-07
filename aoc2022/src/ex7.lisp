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

(defparameter *root-dir* (make-instance 'aoc-dir
                                        :name "/"
                                        :parent nil))
(defparameter *current-dir* *root-dir*)

(defun reset ()
  (setf *root-dir* (make-instance 'aoc-dir
                                          :name "/"
                                        :parent nil))
  (setf *current-dir* *root-dir*))

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

(defun parse-ls-line (line)
  (if (string= (subseq line 0 3) "dir")
      (cons 'dir (subseq line 4))
      (let ((pos (position #\Space line)))
        (cons (parse-integer line :end pos)
              (subseq line (1+ pos))))))

(defun split-on-command (lines)
  (loop :for block = (loop :for (line next . rest) :on lines
                           :while line
                           :for commandp = (char= (char line 0) #\$)
                           :when (and commandp (string= "cd" (subseq line 2 4)))
                             :collect 'cd :into command
                             :and :collect (string-trim " " (subseq line 4)) :into command
                           :when (and commandp (string= "ls" (subseq line 2 4)))
                             :collect 'ls :into command
                           :when (not commandp)
                             :collect (parse-ls-line line) :into command
                           :until (or (not next)
                                      (char= (char next 0) #\$))
                           :finally (setf lines (cons next rest))
                                    (return command))
        :while block
        :collect block))

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

(defun parse-input (file)
  (let* ((lines (read-file-as-lines file))
         (commands (split-on-command lines)))
    (dolist (cmd commands)
      (run-command cmd))))

;;; Questions:
(defmethod size ((dir aoc-dir))
  (let ((size (slot-value dir 'size)))
    (or size
        (let ((total 0))
          (do-hashvalues (subfile (files dir))
            (incf total (size subfile)))
          (setf (slot-value dir 'size) total)))))

(defun all-sizes-below (threshold)
  (let ((acc 0))
    (labels ((aux (file)
               (when (eq (class-of file) (find-class 'aoc-dir))
                 (do-hashvalues (subfile (files file))
                   (aux subfile))
                 (when (< (size file) threshold)
                   (incf acc (size file))))))
      (aux *root-dir*)
      acc)))

(defun answer-ex-7-1 ()
  (reset)
  (parse-input "../inputs/input7")
  (all-sizes-below 100000))

(defun answer-ex-7-2 ())

(defun test ()
  (reset)
  (let ((input '("$ cd /"
                 "$ ls"
                 "dir a"
                 "14848514 b.txt"
                 "8504156 c.dat"
                 "dir d"
                 "$ cd a"
                 "$ ls"
                 "dir e"
                 "29116 f"
                 "2557 g"
                 "62596 h.lst"
                 "$ cd e"
                 "$ ls"
                 "584 i"
                 "$ cd .."
                 "$ cd .."
                 "$ cd d"
                 "$ ls"
                 "4060174 j"
                 "8033020 d.log"
                 "5626152 d.ext"
                 "7214296 k")))
    (dolist (cmd (split-on-command input))
      (run-command cmd))
    (all-sizes-below 100000)))
