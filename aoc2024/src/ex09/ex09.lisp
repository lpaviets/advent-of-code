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

(defclass disk-block ()
  ((id :accessor id :initarg :id)
   (size :accessor size :initarg :size)
   (startpos :accessor startpos :initarg :startpos)
   (filep :accessor filep :initarg :filep)))

(defun make-disk-block (id size startpos filep)
  (make-instance 'disk-block :id id
                             :size size
                             :startpos startpos
                             :filep filep))

;; Return ID, SIZE-OF-BLOCK
(defun read-block (disk start)
  (let* ((id (aref disk start))
         (pos (position id disk :start start :test '/=)))
    (list id (- (or pos (length disk))
                start))))

(defun read-all-blocks (disk)
  (loop :with total-size = (length disk)
        :for block-file-p = t :then (not block-file-p)
        :for start = 0 :then next-start
        :for (id size) = (read-block disk start)
        :for next-start = (+ start size)
        :for block = (make-disk-block id size start block-file-p)
        :collect block :into all-blocks
        :unless block-file-p
          :collect block :into free-blocks
        :while (< next-start total-size)
        :finally (return (list all-blocks free-blocks))))

(defmacro with-disk-block ((id size start filep) block &body body)
  (with-gensyms ((gblock block))
    `(symbol-macrolet ((,id (car ,gblock))
                       (,size (cadr ,gblock))
                       (,start (caddr ,gblock))
                       (,filep (cadddr ,gblock)))
       ,@body)))

;; (defun move-block (file-blocks-rev free-blocks)
;;   (with-disk-block (id size start filep) (aref disk from)
;;     ()
;;     (loop :with remaining = size
;;           :for move-to = to :then (+ to 2)
;;           :while (plusp remaining)
;;           :do (with-disk-block (id-to size-to start-to filep-to)
;;                                (aref disk move-to)
;;                 (when (plusp size-to)
;;                   (setf id-to id)
;;                   (setf filep t)
;;                   (decf size size-to)
;;                   (decf (third (aref disk (1+ to))))
;;                   (incf (fourth )))))))


(defun move-block (file-block-queue free-blocks)
  (destructuring-bind (id size start) (queue-peek file-blocks-queue)
    ()))

(defun answer-ex-9-1 (file))

(defun answer-ex-9-2 (file))
