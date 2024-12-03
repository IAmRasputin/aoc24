(in-package :aoc24/day3)

; I know! I'll use regular expressions! -- Ryan, last words

(defun parse-mul (statement)
  (apply #'* (mapcar #'parse-integer (ppcre:all-matches-as-strings "\\d+" statement))))

(defun part-1 ()
  (let* ((memory (input 3))
         (rx-pattern "mul\\(\\d+,\\d+\\)")
         (mul-statements (ppcre:all-matches-as-strings rx-pattern memory)))
    (reduce #'+ (mapcar #'parse-mul mul-statements))))

(defun part-2 ()
  (let* ((memory (input 3))
         (rx-pattern "mul\\(\\d+,\\d+\\)|do\\(\\)|don't\\(\\)")
         (mul-and-cond-statements (ppcre:all-matches-as-strings rx-pattern memory))
         (sum 0)
         (mul-enabled t))
    (dolist (statement mul-and-cond-statements)
      (cond
        ((string= statement "do()") (setf mul-enabled t))
        ((string= statement "don't()") (setf mul-enabled nil))
        (t (when mul-enabled
             (incf sum (parse-mul statement))))))
    sum))

