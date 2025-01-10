(in-package :aoc24/day7)

(let (cached-input)
  (defun cleaned-input ()
    (or cached-input
        (setf cached-input
              (let* ((raw (input 7))
                     (lines (str:split-omit-nulls #\Newline raw))
                     (equations (mapcar #'parse-integer
                                        (mapcar (lambda (s)
                                                  (str:split-omit-nulls "[: ]" s :regex t))
                                                lines))))
                equations)))))

