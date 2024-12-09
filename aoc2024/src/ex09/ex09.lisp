(in-package #:aoc2024/ex9)

(defparameter *input* #P"input")
(defparameter *test* #P"test")

(defun parse-file (file)
  (let* ((string (read-file-one-line file))
         (len (length string))
         (array (make-array len)))
    (dotimes (i len)
      (setf (aref array i) (digit-char-p (char string i))))
    array))

(defun input-to-memdump (blocks)
  (let* ((total-size (reduce '+ blocks))
         (memdump (make-array total-size :initial-element nil))
         (free-blocks nil)
         (file-blocks nil))
    (loop :for start = 0 :then (+ start size)
          :for size :across blocks
          :for filep = t :then (not filep)
          :for id = 0 :then (if filep id (1+ id))
          :do (dotimes (i size)
                (setf (aref memdump (+ start i)) (cons id filep)))
              (if filep
                  (push (list id size start) file-blocks)
                  (push (list id size start) free-blocks)))
    (values memdump file-blocks (reverse free-blocks))))

(defun move-blocks (memdump)
  (loop :with idx-free = (position nil memdump :key 'cdr)
        :with idx-file = (position t memdump :key 'cdr :from-end t)
        :for id-file = (car (aref memdump idx-file))
        :do (setf (aref memdump idx-free) (cons id-file t))
            (setf (aref memdump idx-file) (cons id-file nil))
            (setf idx-free (position nil memdump :key 'cdr :start (1+ idx-free)))
            (setf idx-file (position t memdump :key 'cdr :end idx-file :from-end t))
        :while (< idx-free idx-file))
  memdump)

(defun show-memdump (memdump)
  (loop :for (id . filep) :across memdump
        :do (if filep
                (format t "~D" id)
                (format t ".")))
  (terpri)
  (force-output))

(defun move-block-at-once (memdump block-to-move free-blocks)
  (loop :with (idm sizem startm) = block-to-move
        :for free-block :in free-blocks
        :for (idf sizef startf) = free-block
        :until (<= startm startf)
        :when (<= sizem sizef)
          :do (dotimes (i sizem)
                (setf (aref memdump (+ startf i)) (cons idm t))
                (setf (cdr (aref memdump (+ startm i))) nil)
                (setf (second free-block) (- sizef sizem)
                      (third free-block) (+ startf sizem)))
              (loop-finish)))

(defun move-all-blocks-at-once (memdump file-blocks free-blocks)
  (loop :for block-to-move :in file-blocks
        :do (move-block-at-once memdump block-to-move free-blocks)
            (when (zerop (second (car free-blocks)))
              (pop free-blocks))        ; small opt: delete first free
            ;;            (show-memdump memdump)
        ))

(defun answer-ex-9-1 (file)
  (let* ((blocks (parse-file file))
         (memdump (input-to-memdump blocks)))
    (move-blocks memdump)
    (loop :for (id . filep) :across memdump
          :for i :from 0
          :while filep
          :sum (* i id))))

(defun answer-ex-9-2 (file)
  (let* ((blocks (parse-file file)))
    (multiple-value-bind (memdump file-blocks free-blocks)
        (input-to-memdump blocks)
      ;;      (format t "Start function:~%")
      ;;      (show-memdump memdump)
      (move-all-blocks-at-once memdump file-blocks free-blocks)
      ;;     (format t "Finished moving blocks:~%")
      ;;     (show-memdump memdump)
      (loop :for (id . filep) :across memdump
            :for i :from 0
            :when filep
              :sum (* i id)))))
