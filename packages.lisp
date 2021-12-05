;;;; package.lisp

(defpackage #:aoc2021
  (:use #:cl)
  (:export
   #:read-file-as-lines
   #:read-file-as-integers
   #:split-word-int
   #:flip
   #:do-array
   #:do-line))

(in-package #:aoc2021)

(defmacro gen-packages ()
  (labels ((mkstr (&rest args)
             (with-output-to-string (s)
               (dolist (a args) (princ a s))))

           (symb (&rest args)
             (values (intern (apply #'mkstr args) :keyword))))

    (let ((list (loop :for i :from 1 :to 25
                      :collect
                      `(defpackage ,(symb :aoc2021 "/EX" i)
                         (:use :cl :aoc2021)
                         (:export
                          ,(symb :answer-ex- i :-1)
                          ,(symb :answer-ex- i :-2))))))
      `(progn
         ,@list))))

(gen-packages)
