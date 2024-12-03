(in-package #:aoc24/day1)

(defun part-1 ()
  (let* ((raw-input (input 1))
         (parsed-input (uiop:split-string raw-input 
                                          :separator '(#\Space #\Newline)))
         (to-numbers (map 'vector #'parse-integer (remove-if (lambda (s) (string= s "")) parsed-input)))
         (column-a (loop for i from 0 to (- (length to-numbers) 1) by 2 collect (aref to-numbers i)))
         (column-b (loop for i from 1 to (- (length to-numbers) 1) by 2 collect (aref to-numbers i)))
         (sorted-a (sort column-a #'<))
         (sorted-b (sort column-b #'<))
         (distance 0))
    (loop for a in sorted-a
          for b in sorted-b
          do (incf distance (abs (- a b))))
    distance))

(defun part-2 ()
  (let* ((raw-input (input 1))
         (parsed-input (uiop:split-string raw-input 
                                          :separator '(#\Space #\Newline)))
         (to-numbers (map 'vector #'parse-integer (remove-if (lambda (s) (string= s "")) parsed-input)))
         (column-a (loop for i from 0 to (- (length to-numbers) 1) by 2 collect (aref to-numbers i)))
         (column-b (loop for i from 1 to (- (length to-numbers) 1) by 2 collect (aref to-numbers i)))
         (sorted-a (sort column-a #'<))
         (sorted-b (sort column-b #'<))
         (distance 0))
    (loop for a in sorted-a do
          (incf distance (* a (count a sorted-b))))
    distance))
