(in-package #:aoc24/day2)

(defun cleaned-input ()
  (let* ((raw-input (input 2))
         (report-strings  (remove-if (lambda (s)
                                       (string= "" s))
                                     (uiop:split-string raw-input :separator '(#\Newline))))
         (raw-reports (mapcar #'uiop:split-string report-strings))
         (reports (mapcar (lambda (r)
                            (mapcar #'parse-integer r))
                          raw-reports)))
    reports))

(defun report-is-safe (report &optional enable-dampener)
  (if (> (length report) 1)
      (let ((prev (car report))
            direction
            problem-found)
        (dolist (reading (cdr report))
          (unless direction
            (setf direction
                  (if (> prev reading)
                      :descending
                      :ascending)))
          (let ((diff (- reading prev)))
            (if (member diff (case direction
                               (:descending '(-1 -2 -3))
                               (:ascending '(1 2 3))))
                (setf prev reading)
                (if (and enable-dampener (not problem-found)) 
                    (setf problem-found t
                          direction nil)
                    (return-from report-is-safe nil)))))
        t) ;; if we made it here, we're good
    t)) ;; can this even happen?


(defun part-1 ()
  (let ((reports (cleaned-input))
        (safe-reports 0))
    (loop for report in reports do
          (when (report-is-safe report)
            (incf safe-reports)))
    safe-reports))

(defun part-2 ()
  (let ((reports (cleaned-input))
        (safe-reports 0))
    (loop for report in reports do
          (when (report-is-safe report t)
            (incf safe-reports)))
    safe-reports))
