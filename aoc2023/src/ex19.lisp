(in-package #:aoc2023/ex19)

(defclass workflow ()
  ((rules :initarg :rules :reader rules)
   (default :initarg :default :reader default)))

(defclass range ()
  ((workflow :initarg :workflow :accessor workflow)
   (ranges :initarg :ranges :accessor ranges))
  (:default-initargs
   :workflow 'in))

(defmethod print-object ((range range) stream)
  (print-unreadable-object (range stream :type t)
    (format stream "WF: ~A RANGES: ~A" (workflow range) (ranges range))))

(defparameter *workflows* (make-hash-table :test 'eq))

(defun parse-rules (rules)
  (let (parsed-rules
        default
        (name (read-from-string rules t nil :end (position #\{ rules))))
    (dolist (rule (ppcre:split "," rules :end (1- (length rules))))
      (if (every 'alpha-char-p rule)
          (setf default (read-from-string rule))
          (ppcre:register-groups-bind ((#'read-from-string label comp num new))
              ("\(\\w\)\([<>]\)\(\\d+\):\(\\w+\)" rule)
            (let ((range (if (eq comp '<)
                             (make-interval 1 num '(t . nil))
                             (make-interval num 4000 '(nil . t)))))
              (push (list label range new) parsed-rules)))))
    (setf (gethash name *workflows*)
          (make-instance 'workflow :rules (nreverse parsed-rules) :default default))))

(defun parse-part (part)
  (let (parsed-part)
    (dolist (p (ppcre:split "," part) (nreverse parsed-part))
      (ppcre:register-groups-bind ((#'read-from-string label val))
          ("\(\\w\)=\(\\d+\)" p)
        (push (list label val) parsed-part)))))

(defun apply-workflow-test (val test)
  (destructuring-bind (range new) test
    (when (interval-contains-p range val)
      new)))

(defun apply-workflow (part workflow)
  (loop :for (label . rule) :in (rules workflow)
        :for val = (second (assoc label part))
        :for new = (apply-workflow-test val rule)
        :thereis new
        :finally (return (default workflow))))

(defun accept-part-p (part)
  (loop :for name = 'in :then (apply-workflow part workflow)
        :for workflow = (gethash name *workflows*)
        :thereis (eq name 'a)
        :never (eq name 'r)))

;; Part 2

(defparameter *ranges* nil)

(defun apply-workflow-rule-range (range test)
  (destructuring-bind (test-range new) test
    (list (list (interval-intersection range test-range) new)
          (list (interval-complement test-range range) nil))))

(defun copy-replace-range (range new-workflow label interval)
  "Returns a new RANGE, where all labels except LABEL have unchanged intervals,
LABEL having INTERVAL instead, and whose workflow is now NEW-WORKFLOW."
  (make-instance 'range
                 :workflow new-workflow
                 :ranges (let ((new-ranges (deepcopy (ranges range))))
                           (setf (second (assoc label new-ranges)) interval)
                           new-ranges)))

(defun apply-workflow-rule-range (range rule)
  (destructuring-bind (label workflow-range new-workflow) rule
    (let* ((cur-label (assoc label (ranges range) :test 'eq))
           (cur-label-range (second cur-label))
           (inter (interval-intersection workflow-range
                                         cur-label-range))
           (compl (car (interval-complement workflow-range
                                            cur-label-range))))
      (when inter
        (push (copy-replace-range range new-workflow label inter) *ranges*))
      (when compl
        (copy-replace-range range nil label compl)))))

(defun split-range-workflow (range workflow)
  (loop :for current = range :then (apply-workflow-rule-range current rule)
        :for rule :in (rules workflow)
        :while current
        :finally (when current
                   (setf (workflow current) (default workflow))
                   (push current *ranges*))))

(defun range-total-size (range)
  (reduce '* (ranges range) :key (lambda (r) (interval-cardinal (second r)))))

(defun answer-ex-19-1 ()
  (destructuring-bind (workflows parts)
      (read-file-as-lines-blocks "../inputs/input19.txt")
    (mapc 'parse-rules workflows)
    (setf parts (mapcar 'parse-part parts))
    (loop :for part :in parts
          :when (accept-part-p part)
            :sum (reduce '+ part :key 'second))))

(defun answer-ex-19-2 ()
  (mapc 'parse-rules (first (read-file-as-lines-blocks "../inputs/input19.txt")))
  (let ((default-interval (make-interval 1 4000)))
    (setf *ranges* (list
                    (make-instance 'range
                                   :workflow 'in
                                   :ranges (list (list 'x default-interval)
                                                 (list 'm default-interval)
                                                 (list 'a default-interval)
                                                 (list 's default-interval))))))
  (loop :for range = (pop *ranges*)
        :for wf = (and range (workflow range))
        :while range
        :if (eq wf 'a)
          :sum (range-total-size range)
        :else
          :unless (eq wf 'r)
            :do (split-range-workflow range (gethash wf *workflows*))))
