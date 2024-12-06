(in-package #:aoc24/day4)

(defvar *input* (let* ((raw-input (input 4))
                       (rows (uiop:split-string raw-input :separator '(#\Newline)))
                       (cleaned-rows (remove "" rows :test #'string=))
                       (grid (make-array (list (length cleaned-rows)
                                               (length (first cleaned-rows)))
                                         :initial-contents cleaned-rows)))
                  grid))

(defun crossing (row col)
  (list
    (loop for r from (- row 1) upto (+ row 1)
          for c from (- col 1) upto (+ col 1) collect (cons r c))
    (loop for r from (+ row 1) downto (- row 1)
          for c from (- col 1) upto (+ col 1) collect (cons r c))))
(defun surrounding (row col)
  (list
    ; up
    (loop for r from row downto (- row 3) collect (cons r col))
    ; down
    (loop for r from row upto (+ row 3) collect (cons r col))
    ; left
    (loop for c from col downto (- col 3) collect (cons row  c))
    ; right
    (loop for c from col upto (+ col 3) collect (cons row c))
    ; upleft
    (loop for r from row downto (- row 3) 
          for c from col downto (- col 3) collect (cons r c))
    ; upright
    (loop for r from row downto (- row 3) 
          for c from col upto (+ col 3) collect (cons r c))
    ; downright
    (loop for r from row upto (+ row 3) 
          for c from col upto (+ col 3) collect (cons r c))
    ; downleft
    (loop for r from row upto (+ row 3) 
          for c from col downto (- col 3) collect (cons r c))))

(defun part-1 ()
  (let ((num-rows (first (array-dimensions *input*)))
        (num-cols (second (array-dimensions *input*)))
        (total 0))
    (dotimes (r num-rows)
      (dotimes (c num-cols)
        (let* ((raw-options (surrounding r c))
               (cleaned-options 
                 (remove-if 
                   (lambda (opt)
                     (dolist (coord opt)
                       (unless (array-in-bounds-p *input* 
                                                  (car coord) 
                                                  (cdr coord))
                         (return t))))
                   raw-options))
               (found 
                 (remove-if-not 
                   (lambda (opt)
                     (string= "XMAS"
                              (coerce 
                                (make-array 4 :initial-contents 
                                            (mapcar (lambda (coord)
                                                      (aref *input* (car coord) (cdr coord)))
                                                    opt))
                                'string)))
                   cleaned-options)))
          (incf total (length found)))))
    total))

;; Eh this kinda sucks but also I don't care and I'm two days behind lmao
(defun part-2 ()
  (let ((num-rows (first (array-dimensions *input*)))
        (num-cols (second (array-dimensions *input*)))
        (total 0))
    (dotimes (r num-rows)
      (dotimes (c num-cols)
        (let* ((possible-x (crossing r c)))
          (when (and (every (lambda (c)
                              (array-in-bounds-p *input* (car c) (cdr c)))
                            (first possible-x))
                     (every (lambda (c)
                              (array-in-bounds-p *input* (car c) (cdr c)))
                            (second possible-x))
                     (or (equal 
                           "SAM"
                           (coerce 
                             (make-array 3 :initial-contents 
                                         (mapcar (lambda (coord)
                                                   (aref *input* (car coord) (cdr coord)))
                                                 (first possible-x)))
                             'string))
                         (equal 
                           "MAS"
                           (coerce 
                             (make-array 3 :initial-contents 
                                         (mapcar (lambda (coord)
                                                   (aref *input* (car coord) (cdr coord)))
                                                 (first possible-x)))
                             'string)))
                     (or (equal 
                           "SAM"
                           (coerce 
                             (make-array 3 :initial-contents 
                                         (mapcar (lambda (coord)
                                                   (aref *input* (car coord) (cdr coord)))
                                                 (second possible-x)))
                             'string))
                         (equal 
                           "MAS"
                           (coerce 
                             (make-array 3 :initial-contents 
                                         (mapcar (lambda (coord)
                                                   (aref *input* (car coord) (cdr coord)))
                                                 (second possible-x)))
                             'string))))
            (incf total)))))
    total))
