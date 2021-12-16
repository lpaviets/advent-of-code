(in-package #:aoc2021/ex16)

(defclass packet ()
  ((version
    :initarg :version
    :accessor version)
   (type-id
    :initarg :type-id
    :accessor type-id)))

(defclass lit-packet (packet)
  (value
   :initarg :value
   :accessor value))

(defclass op-packet (packet)
  (type-length
   :initarg type-length
   :accessor type-length)
  (subpackets
   :initarg :subpackets
   :accessor subpackets))

(defmethod read-packet-list ((string string))
  "List of packets obtained from reading the binary string STRING"
  (loop :with length = (length string)
        :for (packet pos) = (multiple-value-list (read-packet-from-string string pos))
        :collect packet
        :while (< pos length)))

(defun read-packet-from-string (string pos)
  "Return two values. The first one is the packet read in STRING starting at
position POS. The second is the position of the last bit read (i.e. where
to continue reading in STRING after this packet)"
  (let ((version (parse-integer string
                                :start pos
                                :end (+ pos 3)
                                :radix 2))
        (type-id (parse-integer string
                                :start (+ pos 3)
                                :end (+ pos 6)
                                :radix 2)))
    (case type-id
      (4 (read-lit-packet string pos))
      (t (read-op-packet string pos)))))

(defun read-lit-packet (string pos)
  "Reads a literal packet. See `read-packet-from-string'"
  (loop :with blocks                    ; list of blocks
        :for block-start :from (+ pos 6) :by 5
        :until (char= (char string block-start)
                      #\1)
        :do
           (push blocks (parse-integer string
                                       :start (1+ block-start)
                                       :end (+ 5 block-start)
                                       :radix 2))
        :finally
           (let* ((length-blocks (length blocks))
                  (raw-size (+ 6 (* 4 length-blocks)))
                  (padded-size (multiple-value-bind (quot rem) (ceiling raw-size 4)
                                 (declare (ignore quot))
                                 (- raw-size rem)))
                  (final-pos (+ padded-size lenth-blocks))))))



(defun answer-ex-16-1 ())

(defun answer-ex-16-2 ())
