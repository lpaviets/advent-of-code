(in-package #:aoc2023/ex25)

(defun parse-line (line)
  (read-from-string (format nil "(~A)" (remove #\: line))))

(defun make-undirected-graph (edges-list)
  (let ((edges (make-hash-table)))
    (dolist (edge edges-list)
      (loop :with (u . v-list) = edge
            :for v :in v-list
            :do (pushnew (cons v 1) (gethash u edges) :test 'equal)
                (pushnew (cons u 1) (gethash v edges) :test 'equal)))
    (values (lambda (v) (gethash v edges))
            edges)))

(defun answer-ex-25-1 ()
  (let* ((edges-list (read-file-as-lines "../inputs/input25.txt" :parse 'parse-line)))
    (multiple-value-bind (edges-fun edges-table)
        (make-undirected-graph edges-list)
      (do-hashkeys (start edges-table)
        (do-hashkeys (end edges-table)
          (unless (eq start end)
            (multiple-value-bind (max-flow flow capacities saturated)
                (max-flow edges-fun start end)
              (declare (ignore capacities flow))
              (when (plusp max-flow)
                (print (list max-flow (length saturated)))))))))))

(defun answer-ex-25-2 ())
