;;;; Packages.lisp
(asdf/parse-defsystem:defsystem #:aoc2025
  :serial
  t
  :depends-on
  (#:numbra)
  :components
  ((:file "packages")
   (:module org.numbra.perso.aoc::src :pathname "src" :components
    ((:module ex01 :pathname "ex01" :components ((:file "ex01")))
     (:module ex02 :pathname "ex02" :components ((:file "ex02")))
     (:module ex03 :pathname "ex03" :components ((:file "ex03")))
     (:module ex04 :pathname "ex04" :components ((:file "ex04")))
     (:module ex05 :pathname "ex05" :components ((:file "ex05")))
     (:module ex06 :pathname "ex06" :components ((:file "ex06")))
     (:module ex07 :pathname "ex07" :components ((:file "ex07")))
     (:module ex08 :pathname "ex08" :components ((:file "ex08")))
     (:module ex09 :pathname "ex09" :components ((:file "ex09")))
     (:module ex10 :pathname "ex10" :components ((:file "ex10")))
     (:module ex11 :pathname "ex11" :components ((:file "ex11")))
     (:module ex12 :pathname "ex12" :components ((:file "ex12")))))))