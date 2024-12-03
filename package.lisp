;;;; package.lisp

(defpackage #:aoc24/utils
  (:use #:cl)
  (:nicknames :utils)
  (:export :input))

(defpackage #:aoc24/day1
  (:use #:cl)
  (:nicknames :day1)
  (:import-from #:aoc24/utils :input))

