(in-package #:aoc24/day5)

(defvar *input* (let* ((raw-input (input 5))
                       ;; UIOP:SPLIT-STRING can't split on substrings
                       (input-parts (str:split-omit-nulls (format nil "~%~%") raw-input))
                       (page-rules-raw (str:split-omit-nulls #\Newline (first input-parts)))
                       (pages-raw (str:split-omit-nulls #\Newline (second input-parts)))
                       (page-rules-string (mapcar (lambda (rule)
                                                    (str:split-omit-nulls #\| rule))
                                                  page-rules-raw))
                       (pages-string (mapcar (lambda (page)
                                               (str:split-omit-nulls #\, page))
                                             pages-raw))
                       (page-rules (mapcar (lambda (rule)
                                             (mapcar #'parse-integer rule))
                                           page-rules-string))
                       (pages (mapcar (lambda (page)
                                        (mapcar #'parse-integer page))
                                      pages-string)))
                  (cons page-rules pages)))

(defun relevant-rules (update)
  (remove-if-not (lambda (rule)
                   (every (lambda (c)
                            (member c update))
                          rule))
                 (car *input*)))

(defun rule-met (rule update)
  (< (position (car rule) update) 
     (position (cadr rule) update)))

(defun update-compliant (update)
  (every (lambda (r)
           (rule-met r update))
         (relevant-rules update)))

(defun all-compliant-updates ()
  (remove-if-not (lambda (update)
                   (update-compliant update))
                 (cdr *input*)))

(defun middle-page (update)
  (nth (floor (/ (length update) 2)) update))

(defun part-1 ()
  (apply #'+
         (mapcar #'middle-page (all-compliant-updates))))

(apply #'+ (mapcar #'middle-page (all-compliant-updates)))
