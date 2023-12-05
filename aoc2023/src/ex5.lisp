(in-package #:aoc2023/ex5)

(defclass range ()
  ((start :initarg :start :accessor start)
   (end :initarg :end :accessor end)
   (offset :initarg :offset :accessor offset)))

(defclass almanach ()
  ((source :initarg :source :accessor source)
   (destination :initarg :destination :accessor destination)
   (ranges :initarg :ranges :accessor ranges)))

(defun make-range (destination source width)
  (make-instance 'range
                 :start source
                 :end (+ source width -1)
                 :offset (- destination source)))

(defun in-range-p (num range)
  (<= (start range) num (end range)))

(defun range-map-to (num range)
  (if (in-range-p num range)
      (+ num (offset range))
      num))

(defun almanach-map-to (num almanach)
  (let ((range (find-if (lambda (r) (in-range-p num r)) (ranges almanach))))
    (if range
        (range-map-to num range)
        num)))

(defparameter *almanachs* nil)
(defparameter *seeds* nil)

(defun decode-almanach (name num)
  (let ((almanach (cdr (assoc name *almanachs*))))
    (list (destination almanach) (almanach-map-to num almanach))))

(defun find-seed-location (seed)
  (loop :initially (terpri)
        :for (next val) = (list 'seed seed) :then (decode-almanach next val)
        :until (eq next 'location)
        :do (format t "Current: ~A for category ~A~%" val next)
        :finally (return val)))

(defun read-almanach (lines)
  (let* ((category (first lines))
         (ranges (loop :for range :in (rest lines)
                       :collect (apply 'make-range (collect-integers-in-line range))))
         (source-dest (ppcre:register-groups-bind ((#'read-from-string source dest))
                          ("(\\w+)-to-(\\w+) map:" category)
                        (list source dest)))
         (almanach (make-instance 'almanach
                                  :source (first source-dest)
                                  :destination (second source-dest)
                                  :ranges ranges)))
    (push (cons (car source-dest) almanach) *almanachs*)))

(defun read-file (file)
  (let ((blocks (read-file-as-lines-blocks file)))
    (destructuring-bind ((seeds) . almanachs) blocks
      (setf *seeds* (collect-integers-in-line seeds)
            *almanachs* nil)
      (mapc 'read-almanach almanachs))))

(defun answer-ex-5-1 ()
  (read-file "../inputs/input5.txt")
  (argmin *seeds* :key 'find-seed-location))

(defun answer-ex-5-2 ())
