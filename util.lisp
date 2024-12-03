;;;; aoc24.lisp

(in-package #:aoc24/utils)

(defun input (day)
  (uiop:read-file-string (asdf:system-relative-pathname :aoc24 
                                                        (format nil "input/~d" day))))

