(in-package #:aoc2021/ex16)

(defclass packet ()
  ((version
    :initarg :version
    :accessor version)
   (type-id
    :initarg :type-id
    :accessor type-id)))

(defclass lit-packet (packet)
  ((value
    :initarg :value
    :accessor value)))

(defclass op-packet (packet)
  ((type-length
    :initarg :type-length
    :accessor type-length)
   (subpackets
    :initarg :subpackets
    :accessor subpackets)))

(defun hex-to-bin-string (string)
  (format nil "~{~4,'0b~}"
          (loop :for i :from 0 :below (length string)
                :collect (parse-integer string
                                        :start i
                                        :end (1+ i)
                                        :radix 16))))

(defun can-still-read-packet (string pos)
  (let ((length (length string)))
    (and (< (+ pos 7) length)
         (position #\1 string :start pos))))

(defun read-hex-packet (string pos)
  (read-packet (hex-to-bin-string string) pos))

(defun read-packet (string pos)
  "Return two values. The first one is the packet read in STRING starting at
position POS. The second is the position of the last bit read"
  (let ((version (parse-integer string :start pos
                                       :end (+ pos 3)
                                       :radix 2))
        (type-id (parse-integer string :start (+ pos 3)
                                       :end (+ pos 6)
                                       :radix 2)))
    (multiple-value-bind (packet final-pos)
        (case type-id
          (4 (read-lit-packet string pos))
          (t (read-op-packet string pos)))
      (with-accessors ((ver version) (type type-id)) packet
        (setf ver version)
        (setf type type-id))
      (values packet final-pos))))

(defun read-packet-list (string pos &key count length)
  "List of packets obtained from reading the binary string STRING
from the starting position POS
If COUNT is non-nil, read exactly COUNT packets
If LENGTH is non-nil, read at most LENGTH bits from POS
If both are NIL, read the whole string from POS
In any case, also return a second value, which is the position
of the final bit read in STRING"

  (loop :with length-string = (length string)
        :with final-pos = pos
        :for num :from 0

        :while (and (can-still-read-packet string (1+ final-pos))
                    (or (not count)
                        (< num count))
                    (or (not length)
                        (< (- final-pos pos) (1- length))))

        :for (packet new-pos) = (multiple-value-list
                                 (read-packet string pos))
          :then (multiple-value-list
                 (read-packet string (1+ final-pos)))
        :do (setf final-pos new-pos)

        :collect packet :into packets
        :finally (return (values packets final-pos))))

(defun read-lit-packet (string pos)
  "Read a literal packet. See `read-packet-from-string'"
  (loop :for block-start :from (+ pos 6) :by 5
        :collect
        (parse-integer string
                       :start (1+ block-start)
                       :end (+ 5 block-start)
                       :radix 2)
          :into blocks

        :until (char= (char string block-start)
                      #\0)
        :finally
           (let* ((final-pos (+ block-start 4))
                  (lit-string (format nil "~{~4,'0b~}" blocks))
                  (lit-val (parse-integer lit-string :radix 2))
                  (packet (make-instance 'lit-packet
                                         :value lit-val)))
             (return (values packet final-pos)))))

(defun read-op-packet (string pos)
  "Read an operator packet. Dispatches to two other functions
according to the length-type-id.
See `read-packet'"
  (let ((length-type-id (if (char= (char string (+ pos 6)) #\1)
                            1
                            0)))
    (multiple-value-bind (packet final-pos)
        (if (zerop length-type-id)
            (read-op-packet-0 string pos)
            (read-op-packet-1 string pos))
      (setf (type-length packet) length-type-id)
      (values packet final-pos))))

(defun read-op-packet-0 (string pos)
  "Read an operator packet of length-type-ID 0.
 See `read-packet'"
  (let* ((length-subpackets (parse-integer string
                                           :start (+ pos 7)
                                           :end (+ pos 7 15)
                                           :radix 2))
         (subpackets-and-pos (multiple-value-list
                              (read-packet-list string (+ pos 7 15)
                                                :length length-subpackets)))
         (subpackets (first subpackets-and-pos))
         (final-pos (second subpackets-and-pos))
         (packet (make-instance 'op-packet
                                :subpackets subpackets)))
    (values packet final-pos)))

(defun read-op-packet-1 (string pos)
  "Read an operator packet of length-type-ID 1.
 See `read-packet'"
  (let* ((count-subpackets (parse-integer string
                                          :start (+ pos 7)
                                          :end (+ pos 7 11)
                                          :radix 2))
         (subpackets-and-pos (multiple-value-list
                              (read-packet-list string (+ pos 7 11)
                                                :count count-subpackets)))
         (subpackets (first subpackets-and-pos))
         (final-pos (second subpackets-and-pos))
         (packet (make-instance 'op-packet
                                :subpackets subpackets)))
    (values packet final-pos)))

(defmethod sum-packets-version ((packet lit-packet))
  (version packet))

(defmethod sum-packets-version ((packet op-packet))
  (apply '+
         (version packet)
         (mapcar 'sum-packets-version (subpackets packet))))

;;; Part 2

(defmethod operate ((packet lit-packet) &optional (depth 0) debug)
  (when debug
    (dotimes (i depth)
      (format t "| "))
    (format t "Value: ~a~%" (value packet)))
  (value packet))

(defmethod operate ((packet op-packet) &optional (depth 0) debug)
  (when debug
    (dotimes (i depth)
      (format t "| ")))
  (let* ((subpackets (subpackets packet))
         (op (type-id packet))
         (val (if (< op 4)
                  (let ((fun (case op
                               (0 '+)
                               (1 '*)
                               (2 'min)
                               (3 'max))))

                    (when debug
                      (format t "Applying ~a to:~%" fun))

                    (apply fun (mapcar (lambda (x)
                                         (operate x (1+ depth) debug))
                                       subpackets)))
                  (let ((fun (case op
                               (5 '>)
                               (6 '<)
                               (7 '=))))

                    (when debug
                      (format t "Applying ~a to:~%" fun))

                    (if (apply fun (mapcar
                                    (lambda (x)
                                      (operate x (1+ depth) debug))
                                    subpackets))
                        1 0)))))
    (when debug
      (dotimes (i depth)
        (format t "| "))
      (format t "Result: ~a~%" val))
    val))

(defun answer-ex-16-1 ()
  (let* ((line (car (read-file-as-lines "../inputs/input16.txt")))
         (packet (read-hex-packet line 0)))
    (sum-packets-version packet)))

(defun answer-ex-16-2 ()
    (let* ((line (car (read-file-as-lines "../inputs/input16.txt")))
           (packet (read-hex-packet line 0)))
      (operate packet 0 t)))
