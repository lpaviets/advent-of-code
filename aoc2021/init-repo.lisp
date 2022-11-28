;;; Only run this code once
;;; It creates a whole lot of files, filled with some code

(defun create-files-templates ()
  (loop :initially (let ((src-dir (make-pathname :directory '(:relative "src"))))
                     (ensure-directories-exist src-dir))
        :for i :from 1 :to 25
        :for filename = (make-pathname :directory '(:relative "src")
                                       :name (format nil "ex~a" i)
                                       :type "lisp")
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
