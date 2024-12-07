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

(defun list-swap (lst first-idx second-idx)
  (let* ((list-copy (copy-list lst))
         (first-temp (nth first-idx list-copy))
         (second-temp (nth second-idx list-copy)))
    (setf (nth second-idx list-copy) first-temp
          (nth first-idx list-copy) second-temp)
    list-copy))


(defun fix-update (update)
  (let ((to-fix (copy-list update))
        (rules (relevant-rules update)))
    (labels ((comp (x y)
               (if (and (member (list y x) rules :test #'equal)
                        (member (list x y) rules :test #'equal))
                   t
                   (member (list x y) rules :test #'equal))))
      (sort to-fix #'comp))))

(let ((rule-cache (make-hash-table :test #'equal)))
  (defun relevant-rules (update)
    (let ((hash (gethash update rule-cache)))
      (or hash
          (setf hash (remove-if-not (lambda (rule)
                                      (every (lambda (c)
                                               (member c update))
                                             rule))
                                    (car *input*)))))))

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

(defun all-noncompliant-updates ()
  (remove-if (lambda (update)
               (update-compliant update))
             (cdr *input*)))

(defun middle-page (update)
  (nth (floor (/ (length update) 2)) update))

(defun part-1 ()
  (apply #'+
         (mapcar #'middle-page (all-compliant-updates))))

(defun part-2 ()
  (apply #'+
         (mapcar #'middle-page (mapcar #'fix-update (all-noncompliant-updates)))))
