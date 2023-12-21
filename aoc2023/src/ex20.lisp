(in-package #:aoc2023/ex20)

(defparameter *debug* nil)
(defparameter *modules-table* nil)

(defparameter *pulses* nil
  "Queue of the pulses to handle. A pulse is a cons (MODULE . HIGH),
  where MODULE is a module name and HIGH is T or NIL.")

(defparameter *low-pulses* 0)
(defparameter *high-pulses* 0)

;; For part 2
(defparameter *step* 0)
(defparameter *preperiod-to-rx* nil)

(defclass pulse ()
  ((high :initarg :high :reader high)
   (source :initarg :source :reader source)
   (destination :initarg :destination :reader destination)))

(defmethod print-object ((pulse pulse) stream)
  (print-unreadable-object (pulse stream :type t)
    (format stream "~A -~:[low ~;high~] -> ~A"
            (if (typep (source pulse) 'module)
                (name (source pulse))
                (source pulse))
            (high pulse)
            (if (typep (destination pulse) 'module)
                (name (destination pulse))
                (destination pulse)))))

(defun make-pulse (from to high)
  (make-instance 'pulse
                 :source from
                 :destination to
                 :high high))

(defclass module ()
  ((name :initarg :name :reader name)
   (destinations :initarg :destinations :accessor destinations)
   (sources :initform nil :accessor sources)))

(defclass flip-flop (module)
  ((on :initform nil :accessor flip-flop-on)))

(defclass conjunction (module)
  ((last-pulse :initform nil :accessor last-pulse)))

(defclass broadcaster (module)
  ())


(defun module-by-name (name)
  (cdr (assoc name *modules-table*)))

(defun flip (module)
  (assert (typep module 'flip-flop))
  (setf (flip-flop-on module) (not (flip-flop-on module))))

(defun %parse-module (line)
  (let* ((splitted (ppcre:split "\( -> |,\)" line))
         (destinations (mapcar 'read-from-string (cdr splitted)))
         (type-and-name (car splitted))
         (type (char type-and-name 0))
         (name (if (alpha-char-p type)
                   (read-from-string type-and-name t nil)
                   (read-from-string type-and-name t nil :start 1)))
         (module (make-instance (case type
                                  (#\& 'conjunction)
                                  (#\% 'flip-flop)
                                  (t 'broadcaster))
                                :destinations destinations
                                :name name)))
    module))

(defun parse-modules (lines)
  (setf *modules-table* nil)
  (dolist (line lines)
    (let ((module (%parse-module line)))
      (push (cons (name module) module) *modules-table*)))
  (loop :for (name . module) :in *modules-table*
        :do (setf (destinations module)
                  (mapcar (lambda (mod-name)
                            (let ((mod (module-by-name mod-name)))
                              (when mod
                                (pushnew module (sources mod)))
                              (when (and mod (typep mod 'conjunction))
                                (pushnew (cons module nil) (last-pulse mod)
                                         :test 'equal))
                              (or mod (make-instance 'module :name mod-name))))
                          (destinations module)))))

(defun send-pulse (from to high)
  (let ((pulse (make-pulse from to high)))
    (queue-push *pulses* pulse)
    (when *debug* (format t "~&Pushing ~A~%" pulse))
    (if high (incf *high-pulses*) (incf *low-pulses*))
    pulse))

(defun process-pulse (pulse)
  (with-accessors ((high high)
                   (source source)
                   (dest destination))
      pulse
    (receive-pulse dest source :high high)))

(defmethod receive-pulse :around ((module module) from &key high)
  (when *debug*
    (format t "~&Process ~A ~24T-~:[low ~;high~] -> ~A"
            (when (and from (typep from 'module))
              (name from))
            high
            (name module)))
  (call-next-method module from :high high))

(defmethod receive-pulse ((module module) from &key high)
  (when (and (eq 'rx (name module))
             (not high))
    (setf *reached-rx* t)))

(defmethod receive-pulse ((broadcaster broadcaster) from &key high)
  (declare (ignore from))
  (dolist (to (destinations broadcaster))
    (send-pulse broadcaster to high)))

(defmethod receive-pulse ((module flip-flop) from &key high)
  (declare (ignore from))
  (unless high                  ; do nothing on HIGH (i.e., HIGH is T)
    (let* ((on (flip-flop-on module))
           (new-high (if on nil t)))
      (dolist (to (destinations module))
        (send-pulse module to new-high))
      (flip module))))

(defmethod receive-pulse ((module conjunction) from &key high)
  (setf (last-pulse-from module from) high)
  (let ((new-high (not (loop :for (mod-from . mod-last) :in (last-pulse module)
                             :always mod-last))))
    (dolist (to (destinations module))
      (send-pulse module to new-high))))

(defmethod receive-pulse :before ((module conjunction) from &key high)
  (when (and (eq (name module) 'dn)
             high)
    (let ((period *step*)
          (old (assoc from *preperiod-to-rx*)))
      (unless old
        (push (cons from period) *preperiod-to-rx*)))))

(defun last-pulse-from (conjunction from)
  (cdr (assoc from (last-pulse conjunction) :test 'eq)))

(defun (setf last-pulse-from) (pulse conjunction from)
  (let ((present (assoc from (last-pulse conjunction) :test 'eq)))
    (if present
        (setf (cdr present) pulse)
        (push (cons from pulse) (last-pulse conjunction)))))

(defun parse-file-reset (file)
  (setf *modules-table* nil)
  (parse-modules (read-file-as-lines file))
  (setf *low-pulses* 0
        *high-pulses* 0
        *step* 0
        *all-periodic* t
        *preperiod-to-rx* nil))

(defun press-button ()
  (setf *pulses* (make-queue))
  (let ((broadcaster (module-by-name 'broadcaster)))
    (send-pulse nil broadcaster nil))
  (loop :for pulse = (queue-pop *pulses*)
        :do (process-pulse pulse)
        :until (queue-empty-p *pulses*)))

(defun answer-ex-20-1 ()
  (parse-file-reset "../inputs/input20.txt")
  (dotimes (i 1000)
    (press-button))
  (* *low-pulses* *high-pulses*))

(defun answer-ex-20-2 ()
  (parse-file-reset "../inputs/input20.txt")
  (loop :with num-periods = (length (sources (module-by-name 'dn)))
        :do (incf *step*)
            (press-button)
        :thereis (and (= (length *preperiod-to-rx*) num-periods)
                      (reduce 'lcm *preperiod-to-rx* :key 'cdr))))
