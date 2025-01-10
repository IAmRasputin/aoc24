;;;; package.lisp

(defpackage #:aoc24/utils
  (:use #:cl)
  (:nicknames :utils)
  (:export :input))

(defpackage #:aoc24/day1
  (:use #:cl)
  (:nicknames :day1)
  (:import-from #:aoc24/utils :input))

(defpackage #:aoc24/day2
  (:use #:cl)
  (:nicknames :day2)
  (:import-from #:aoc24/utils :input))

(defpackage #:aoc24/day3
  (:use #:cl)
  (:nicknames :day3)
  (:import-from #:aoc24/utils :input))

(defpackage #:aoc24/day4
  (:use #:cl)
  (:nicknames :day4)
  (:import-from #:aoc24/utils :input))

(defpackage #:aoc24/day5
  (:use #:cl)
  (:nicknames :day5)
  (:import-from #:aoc24/utils :input))

(defpackage #:aoc24/day6
  (:use #:cl)
  (:nicknames :day6)
  (:import-from #:aoc24/utils :input))

(defpackage #:aoc24/day7
  (:use #:cl)
  (:nicknames :day7)
  (:import-from #:aoc24/utils :input))
