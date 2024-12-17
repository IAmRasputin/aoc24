(in-package #:aoc24/day6)


(defvar *input* (let* ((raw-input (input 6))
                       (raw-input-lines (remove "" (str:split #\Newline raw-input) :test #'equal)))
                  (make-array (list (length raw-input-lines)
                                    (length (car raw-input-lines)))
                              :initial-contents raw-input-lines)))

(defvar *total-rows* (first (array-dimensions *input*)))
(defvar *total-cols* (second (array-dimensions *input*)))
(defvar *guard-row*)
(defvar *guard-col*)
(defvar *guard-start* (dotimes (r *total-rows*)
                        (dotimes (c *total-cols*)
                          (when (eq (aref *input* r c) #\^)
                            (setf (aref *input* r c) #\.)
                            (setf *guard-row* r
                                  *guard-col* c)
                            (return (cons r c))))))

(defvar *direction* :north)
(defvar *spots-visited* (list (cons *guard-row* *guard-col*)))

(defun reset ()
  (setf *input* (let* ((raw-input (input 6))
                         (raw-input-lines (remove "" (str:split #\Newline raw-input) :test #'equal)))
                    (make-array (list (length raw-input-lines)
                                      (length (car raw-input-lines)))
                                :initial-contents raw-input-lines)))

  (setf *total-rows* (first (array-dimensions *input*)))
  (setf *total-cols* (second (array-dimensions *input*)))
  (setf *guard-start* (dotimes (r *total-rows*)
                          (dotimes (c *total-cols*)
                            (when (eq (aref *input* r c) #\^)
                              (setf (aref *input* r c) #\.)
                              (setf *guard-row* r
                                    *guard-col* c)
                              (return (cons r c))))))
  (setf *direction* :north)
  (setf *spots-visited* (list (cons *guard-row* *guard-col*))))

(defmacro with-fresh-state (&body body)
  `(let* ((*input* (let* ((raw-input (input 6))
                          (raw-input-lines (remove "" (str:split #\Newline raw-input) :test #'equal)))
                     (make-array (list (length raw-input-lines)
                                       (length (car raw-input-lines)))
                                 :initial-contents raw-input-lines)))

          (*total-rows* (first (array-dimensions *input*)))
          (*total-cols* (second (array-dimensions *input*)))
          (*guard-start* (dotimes (r *total-rows*)
                           (dotimes (c *total-cols*)
                             (when (eq (aref *input* r c) #\^)
                               (setf (aref *input* r c) #\.)
                               (setf *guard-row* r
                                     *guard-col* c)
                               (return (cons r c))))))
          (*direction* :north)
          (*spots-visited* (list (cons *guard-row* *guard-col*))))
     ,@body))

(defun turn () 
  (setf *direction* (ccase *direction*
                           (:north :east)
                           (:east :south)
                           (:south :west)
                           (:west :north))))


(defun move ()
  (case *direction*
    (:north (cond
              ((= *guard-row* 0) :exit)
              ((char= (aref *floor-map* (- *guard-row* 1) *guard-col*) #\#) :blocked)
              (t (progn
                   (decf *guard-row*)
                   (pushnew (cons *guard-row* *guard-col*) *spots-visited* :test #'equal)
                   (cons *guard-row* *guard-col*)))))
    (:east (cond
             ((= *guard-col* (- *total-cols* 1)) :exit)
             ((char= (aref *floor-map* *guard-row* (1+ *guard-col*)) #\#) :blocked)
             (t (progn
                  (incf *guard-col*)
                  (pushnew (cons *guard-row* *guard-col*) *spots-visited* :test #'equal)
                  (cons *guard-row* *guard-col*)))))
    (:south (cond
              ((= *guard-row* (- *total-rows* 1)) :exit)
              ((char= (aref *floor-map* (+ *guard-row* 1) *guard-col*) #\#) :blocked)
              (t (progn
                   (incf *guard-row*)
                   (pushnew (cons *guard-row* *guard-col*) *spots-visited* :test #'equal)
                   (cons *guard-row* *guard-col*)))))
    (:west (cond
             ((= *guard-col* 0) :exit)
             ((char= (aref *floor-map* *guard-row* (- *guard-col* 1)) #\#) :blocked)
             (t (progn
                  (decf *guard-col*)
                  (pushnew (cons *guard-row* *guard-col*) *spots-visited* :test #'equal)
                  (cons *guard-row* *guard-col*)))))))

(defun debug-log ()
  (labels ((at (row col)
             (if (array-in-bounds-p *input* row col)
                 (if (or (char= #\. (aref *input* row col))
                         (char= #\^ (aref *input* row col)))
                     #\.
                     (aref *input* row col))
                 #\Space)))
    (format t "---~%")
    (format t "~a~a~a~%" 
            (at (- *guard-row* 1) (- *guard-col* 1))
            (at (- *guard-row* 1) *guard-col*)
            (at (- *guard-row* 1) (+ *guard-col* 1)))
    (format t "~a~a~a~%"
            (at *guard-row* (- *guard-col* 1))
            (case *direction*
              (:north "^")
              (:south "v")
              (:east ">")
              (:west "<"))
            (at *guard-row* (+ *guard-col* 1)))
    (format t "~a~a~a~%"
            (at (+ *guard-row* 1) (- *guard-col* 1))
            (at (+ *guard-row* 1) *guard-col*)
            (at (+ *guard-row* 1) (+ *guard-col* 1)))
    (format t "---~%")
    (format t "(~d, ~d) (visited: ~d)~%" *guard-row* *guard-col* (length *spots-visited*))))

(defun guard-step ()
  (let ((result (move)))
    (case result
      (:exit :exit)
      (:blocked (turn))
      (t result))))

(defun is-loop (obst-row obst-col)
  (with-fresh-state
    (unless (and (eq obst-row *guard-row*)
                 (eq obst-col *guard-col*))
      (setf (aref *input* obst-row obst-col) #\#)
      (do ((result (guard-step) (guard-step)))
          ((member result '(:exit :loop)) (eq result :loop))))))

(defun part-1 ()
  (do ((result (guard-step) (guard-step)))
      ((eq result :exit) (length *spots-visited*))))

(defun part-2 ()
  (let ((loops 0))
    (dotimes (r *total-rows*)
      (dotimes (c *total-cols*)
        (when (is-loop r c) 
          (incf loops))))))

(part-2)
