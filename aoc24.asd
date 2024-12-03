;;;; aoc24.asd

(asdf:defsystem #:aoc24
  :description "Advent of Code, 2024"
  :author "Ryan Gannon <ryanmgannon@gmail.com>"
  :license  "GPLv3"
  :version "0.0.1"
  :serial t
  :depends-on ("advent") ;; github.com/IAmRasputin/advent
  :components ((:file "package")
               (:file "util")
               (:file "day1")))
