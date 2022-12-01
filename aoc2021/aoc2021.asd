;;;; aoc2021.asd

(asdf:defsystem #:aoc2021
  :description "Solutions for the 2021 Advent of Code"
  :author "Léo Paviet Salomon"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:numbra)
  :components ((:file "packages")
               (:module src
                :pathname "src"
                :components ((:file "ex1")
                             (:file "ex2")
                             (:file "ex3")
                             (:file "ex4")
                             (:file "ex5")
                             (:file "ex6")
                             (:file "ex7")
                             (:file "ex8")
                             (:file "ex9")
                             (:file "ex10")
                             (:file "ex11")
                             (:file "ex12")
                             (:file "ex13")
                             (:file "ex14")
                             (:file "ex15")
                             (:file "ex16")
                             (:file "ex17")
                             (:file "ex18")
                             (:file "ex19")
                             (:file "ex20")
                             (:file "ex21")
                             (:file "ex22")
                             (:file "ex23")
                             (:file "ex24")
                             (:file "ex25")))))
