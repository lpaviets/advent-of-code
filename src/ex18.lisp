(in-package #:aoc2021/ex18)

(defclass fish ()
  ((left
    :accessor left
    :initarg :left
    :type (or integer fish))
   (right
    :accessor right
    :initarg :right
    :type (or integer fish))
   (depth
    :accessor depth
    :initarg :depth
    :type integer)
   (parent
    :accessor parent
    :initarg :parent
    :initform nil
    :type (or null fish))))

(defun print-fish (fish)
  (let ((pad (make-string (depth fish)
                          :initial-element #\Space)))
    (format t "~a(~%" pad)
    (if (integerp (left fish))
        (format t "~a ~a~%" pad (left fish))
        (print-fish (left fish)))
    (if (integerp (right fish))
        (format t "~a ~a~%~a)~%" pad (right fish) pad)
        (print-fish (right fish)))))

(defmethod parse-fish ((stream stream) &optional (depth 0) parent)
  (if (char= (peek-char t stream) #\[)
      (parse-array stream depth parent)
      (parse-value stream)))

(defmethod parse-fish ((string string) &optional (depth 0) parent)
  (with-input-from-string (s string)
    (parse-fish s depth parent)))

(defun parse-value (stream)
  (parse-digit (read-char stream)))

(defun parse-array (stream &optional (depth 0) parent)
  (read-char stream)
  (let* ((fish (make-instance 'fish
                              :depth depth
                              :parent parent))
         (left (parse-fish stream (1+ depth) fish)))
    (assert (char= (read-char stream) #\,))
    (setf (left fish) left)
    (setf (right fish) (parse-fish stream (1+ depth) fish))
    (assert (char= (read-char stream) #\]))
    fish))

(defun get-leaves (fish leaves)
  (cond
    ((integerp fish) leaves)
    ((and (integerp (left fish))
          (integerp (right fish)))
     (push fish leaves))
    (t (let ((left-leaves (get-leaves (left fish) leaves)))
         (get-leaves (right fish) left-leaves)))))

(defun left-son-p (fish)
  (eq fish (left (parent fish))))

(defun up-until-son-dir (fish val dir)
  (let ((fun (if (eq dir :left) 'left 'right)))
    (loop :with start-fish = (if (eq fish (left (parent fish)))
                                 (parent fish)
                                 (parent (parent fish)))
          :until )))

(defun add-val-down (fish val dir)
  (let ((fun (if (eq dir :left) 'left 'right)))
    (loop :for cur-fish = fish :then (funcall fun cur-fish)
          :until (integerp (funcall fun cur-fish))
          :finally (if (eq dir :left)
                       (incf (left cur-fish) val)
                       (incf (right cur-fish) val)))))

(defun explode-subfish (fish)
  (let ((deepest (nreverse (get-leaves fish nil))))
    (loop :for x :on deepest
          :while x
          :when (<= 4 (depth x)) :do
            (let ((lval (left x))
                  (rval (right x)))
              (if (eq x (left (parent x)))
                  (setf (left (parent x)) 0)
                  (setf (right (parent x)) 0)))
            (return t))))

(defun answer-ex-18-1 ())

(defun answer-ex-18-2 ())
