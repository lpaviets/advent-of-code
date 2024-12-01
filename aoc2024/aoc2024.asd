;;;; Packages.lisp
(asdf:defsystem #:aoc2024
  :serial t
  :depends-on (#:numbra)
  :components ((:file "packages")
               (:module src
                :pathname "src"
                :components
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
                 (:module ex12 :pathname "ex12" :components ((:file "ex12")))
                 (:module ex13 :pathname "ex13" :components ((:file "ex13")))
                 (:module ex14 :pathname "ex14" :components ((:file "ex14")))
                 (:module ex15 :pathname "ex15" :components ((:file "ex15")))
                 (:module ex16 :pathname "ex16" :components ((:file "ex16")))
                 (:module ex17 :pathname "ex17" :components ((:file "ex17")))
                 (:module ex18 :pathname "ex18" :components ((:file "ex18")))
                 (:module ex19 :pathname "ex19" :components ((:file "ex19")))
                 (:module ex20 :pathname "ex20" :components ((:file "ex20")))
                 (:module ex21 :pathname "ex21" :components ((:file "ex21")))
                 (:module ex22 :pathname "ex22" :components ((:file "ex22")))
                 (:module ex23 :pathname "ex23" :components ((:file "ex23")))
                 (:module ex24 :pathname "ex24" :components ((:file "ex24")))
                 (:module ex25 :pathname "ex25" :components ((:file "ex25")))))))
