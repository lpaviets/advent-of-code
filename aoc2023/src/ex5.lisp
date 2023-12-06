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

(defun parse-seeds (seeds)
  (setf *seeds* (collect-integers-in-line seeds)))

(defun read-file (file)
  (let ((blocks (read-file-as-lines-blocks file)))
    (destructuring-bind ((seeds) . almanachs) blocks
      (parse-seeds seeds)
      (setf *almanachs* nil)
      (mapc 'read-almanach (reverse almanachs)))))

;; Interval is a pair
;; Returns a pair (MODIFIED . UNCHANGED)
;; where MODIFIED is an interval that has
;; been "remapped" by RANGE, and UNCHANGED is a list of
;; unchanged intervals that might remain to be mapped by
;; other ranges in the same map.

;; Case 1: |---------|
;;         a         b
;;       |-------------|
;;       s             e
;; Returns: ([a+offset, b+offset])
;;
;; Case 2: |---------|
;;         a         b
;;           |-------------|
;;           s             e
;; Returns: ([s+offset, b+offset], [a, s])
;;
;; Case 3: |---------|
;;         a         b
;;       |--------|
;;       s        e
;; Returns: ([a+offset, e+offset], [e, b])
;;
;; Case 4: |---------|
;;         a         b
;;           |-----|
;;           s     e
;; Returns: ([s+offset, e+offset], [a, s], [e, b])
;;
;; Case 5: |---------|
;;         a         b
;;                       |-------------|
;;                       s             e
;; Returns: (NIL, [a, b])

(defun map-interval-range (interval range)
  (let ((a (first interval))
        (b (second interval))
        (s (start range))
        (e (end range))
        (offset (offset range)))
    (let ((result (cond
                    ((<= s a b e) (list (list (+ a offset) (+ b offset))))
                    ((<= a s b e) (list (list (+ s offset) (+ b offset))
                                        (list a s)))
                    ((<= s a e b) (list (list (+ a offset) (+ e offset))
                                        (list e b)))
                    ((<= a s e b) (list (list (+ s offset) (+ e offset))
                                        (list a s)
                                        (list e b)))
                    (t (list nil (list a b))))))
      result)))

(defun map-interval-almanach (interval almanach)
  (loop :with remaining = (list interval)
        :with valids = nil
        :for range :in (ranges almanach)
        :do (let ((new-remaining nil))
              (dolist (int remaining)
                (destructuring-bind (new-valid . rest)
                    (map-interval-range int range)
                  (push new-valid valids)
                  (setf new-remaining (nconc rest new-remaining))))
              (setf remaining new-remaining))
        :finally (return (remove nil (append valids remaining)))))

(defun map-all-intervals-almanach (intervals almanach)
  (mapcan (lambda (int)
            (map-interval-almanach int almanach))
          intervals))

(defun answer-ex-5-1 ()
  (read-file "../inputs/input5.txt")
  (argmin *seeds* :key 'find-seed-location))

(defun answer-ex-5-2 ()
  (read-file "../inputs/input5.txt")
  (let* ((seeds-ranges (loop :for (start width) :on *seeds* by #'cddr
                             :collect (list start (+ start width -1))))
         (locations (reduce 'map-all-intervals-almanach *almanachs*
                            :initial-value seeds-ranges
                            :key 'cdr)))
    (loop :for (a b) :in locations
          :minimize a)))
