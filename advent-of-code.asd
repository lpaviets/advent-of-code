;;; advent-of-code.asd

(asdf:defsystem #:advent-of-code
  :description "Generic utilities for Advent of Code"
  :author "Léo Paviet Salomon"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-ppcre #:heap)
  :components ((:file "packages")
               (:file "heap+")
               (:file "parsing")
               (:file "utilities")
               (:file "macros")
               (:file "algorithms")
               (:file "printing")))
