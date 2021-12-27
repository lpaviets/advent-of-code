(defpackage #:advent-of-code
  (:use #:cl)
  (:export
   ;; Generate sub-packages
   #:gen-packages
   ;; parsing
   #:read-file-as-lines
   #:read-file-one-line
   #:read-file-as-integers
   #:read-array
   ;; Small parsing utils
   #:coma-separated-int-line
   #:split-word-int
   #:parse-digit
   ;; Generic utilities
   #:neighbours
   #:flip
   #:range
   #:permutations
   #:sublists-length
   #:extremum
   #:extremum-array
   #:deepcopy
   #:shuffle
   ;; Algorithms
   #:shortest-path
   #:shortest-path-dec-key
   #:dfs
   ;; Macros
   #:do-array
   #:do-line
   ;; Print
   #:print-array
   #:print-hash))

(in-package #:advent-of-code)

(defmacro gen-packages (meta-package)
  (labels ((mkstr (&rest args)
             (with-output-to-string (s)
               (dolist (a args) (princ a s))))

           (symb (&rest args)
             (values (intern (apply #'mkstr args) :keyword))))

    (let ((list (loop :for i :from 1 :to 25
                      :collect
                      `(defpackage ,(symb meta-package "/EX" i)
                         (:use :cl ,meta-package)
                         (:export
                          ,(symb :answer-ex- i :-1)
                          ,(symb :answer-ex- i :-2))))))
      `(progn
         ,@list))))
