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
    :initform 0
    :type integer)
   (parent
    :accessor parent
    :initarg :parent
    :initform nil
    :type (or null fish))
   (side
    :accessor side
    :initarg :side
    :initform nil
    :type symbol)))

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

(defmethod parse-fish ((stream stream) &optional (depth 0) parent side)
  (if (char= (peek-char t stream) #\[)
      (parse-array stream depth parent side)
      (parse-value stream)))

(defmethod parse-fish ((string string) &optional (depth 0) parent side)
  (with-input-from-string (s string)
    (parse-fish s depth parent side)))

(defun parse-value (stream)
  (parse-digit (read-char stream)))

(defun parse-array (stream &optional (depth 0) parent side)
  (read-char stream)
  (let* ((fish (make-instance 'fish
                              :depth depth
                              :parent parent
                              :side side))
         (left (parse-fish stream (1+ depth) fish 'left)))
    (assert (char= (read-char stream) #\,))
    (setf (left fish) left)
    (setf (right fish) (parse-fish stream (1+ depth) fish 'right))
    (assert (char= (read-char stream) #\]))
    fish))

(defun propagate-explosion (fish explosion)
  (destructuring-bind (lval rval) explosion
    ))

(defun explode-fish (fish)
  (unless (integerp fish)
    (if (= (depth fish) 4)              ; Base case: explode this fish
        (let ((left (left fish))
              (right (right fish))
              (side (side fish))
              (parent (parent fish)))

          (if (eq side 'left)           ; Start by deleting this node
              (setf (left parent) 0)
              (setf (right parent) 0))

          (cons left right))

        (let ((explode-left (explode-fish (left fish))))
          ;; Try to fix the right side of the explosion
          (when (and (consp explode-left)
                     (cdr explode-left))
            (if (integerp (right fish))
                (incf (right fish) (cdr explode-left))
                (loop :for cur-fish = (right fish)
                        :then (left cur-fish)
                      :until (integerp (left cur-fish))
                      :finally
                         (incf (left cur-fish) (cdr explode-left))))
            (setf (cdr explode-left) nil))

          (if (consp explode-left)
              explode-left              ; Propagate info upwards
              (let ((explode-right (explode-fish (right fish))))
                (when (and (consp explode-right)
                           (car explode-right))
                  (if (integerp (left fish))
                      (incf (left fish) (car explode-right))
                      (loop :for cur-fish = (left fish)
                              :then (right cur-fish)
                            :until (integerp (right cur-fish))
                            :finally
                               (incf (right cur-fish) (car explode-right))))
                  (setf (car explode-right) nil))
                explode-right))))))

(defmethod split-fish ((val integer))
  (if (<= 10 val)
      (let* ((half (truncate val 2))
             (fish (make-instance 'fish
                                  :left half
                                  :right (- val half))))
        (cons fish t))))

(defmethod split-fish ((fish fish))
  (with-accessors ((left left)
                   (right right)
                   (depth depth))
      fish
    (let ((left-split (split-fish left)))
      (if (cdr left-split)
          (progn
            (setf left (car left-split))
            (setf (depth left) (1+ depth)
                  (side left) 'left
                  (parent left) fish)
            (cons fish t))
          (let ((right-split (split-fish right)))
            (if (cdr right-split)
                (progn
                  (setf right (car right-split))
                  (setf (depth right) (1+ depth)
                        (side right) 'right
                        (parent right) fish)
                  (cons fish t))
                (cons fish nil)))))))

(defun reduce-fish (fish)
  (loop :while (or (explode-fish fish)
                   (cdr (split-fish fish))))
  fish)

(defun increase-depth (fish &optional (val 1))
  (unless (integerp fish)
    (incf (depth fish) val)
    (increase-depth (left fish) val)
    (increase-depth (right fish) val)))

(defun merge-fish (fisha fishb)
  (let ((fish (make-instance 'fish
                             :left fisha
                             :right fishb)))
    (setf (parent fisha) fish
          (parent fishb) fish
          (side fisha) 'left
          (side fishb) 'right)
    (increase-depth fisha 1)
    (increase-depth fishb 1)
    fish))

(defmethod magnitude ((val integer))
  val)

(defmethod magnitude ((fish fish))
  (+ (* 3 (magnitude (left fish)))
     (* 2 (magnitude (right fish)))))

(defun add-all-fishes (fishes)
  (reduce-fish (reduce (lambda (x y)
                         (reduce-fish (merge-fish x y)))
                       fishes)))

(defmethod copy-fish ((val integer))
  val)

(defmethod copy-fish ((fish fish))
  (with-accessors ((left left)
                   (right right)
                   (depth depth)
                   (side side)
                   (parent parent))
      fish
    (let* ((lfish (copy-fish left))
           (rfish (copy-fish right))
           (fish (make-instance 'fish
                                :depth depth
                                :side side
                                :parent parent
                                :left lfish
                                :right rfish)))
      (unless (integerp lfish)
        (setf (parent lfish) fish))
      (unless (integerp rfish)
        (setf (parent rfish) fish))
      fish)))

(defun answer-ex-18-1 ()
  (let ((fishes (read-file-as-lines "../inputs/input18.txt"
                                    :parse 'parse-fish)))
    (magnitude (add-all-fishes fishes))))

(defun answer-ex-18-2 ()
  (let ((fishes (read-file-as-lines "../inputs/input18.txt"
                                    :parse 'parse-fish)))
    (loop :for fisha :in fishes
          :for i :from 0
          :maximize
          (loop :for fishb :in fishes
                :for j :from 0
                :unless (= i j)
                  :maximize
                  (magnitude (add-all-fishes (list (copy-fish fisha)
                                                   (copy-fish fishb))))))))
