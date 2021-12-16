;;; Only run this code once
;;; It creates a whole lot of files, filled with some code

(defun create-files-templates ()
  (loop :initially (ensure-directories-exist "src/")
        :for i :from 1 :to 25
        :for filename = (format nil "src/ex~a.lisp" i)
        :unless (probe-file filename)
          :do
             (format t "Creating file ~a~%" filename)
             (with-open-file (s
                              filename
                              :direction :output
                              :if-exists :error
                              :if-does-not-exist :create)
               (format s "(in-package #:aoc2021/ex~a)~%~%" i)
               (format s "(defun answer-ex-~a-1 ())~%~%" i)
               (format s "(defun answer-ex-~a-2 ())~%~%" i))))
