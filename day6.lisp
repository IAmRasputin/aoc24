(in-package #:aoc24/day6)

;; haha wow this sucks, but I've been stuck here so I just want it to work, you know?
;; this may have been a job for OOP after all
(let (cached-input)
  (defun cleaned-input ()
    (or cached-input
        (setf cached-input 
              (let* ((raw-input (input 6))
                     (raw-input-lines (remove "" (str:split #\Newline raw-input) :test #'equal)))
                (make-array (list (length raw-input-lines)
                                  (length (car raw-input-lines)))
                            :initial-contents raw-input-lines))))))

(defclass room-state ()
  ((floor-map :initform (cleaned-input)
              :accessor room-input-map
              :initarg :input-map)
  (num-rows :accessor room-rows
             :initarg :num-rows)
   (num-cols :accessor room-cols
             :initarg :num-cols)
   (guard-row :accessor room-guard-row
              :initarg :guard-row)
   (guard-col :accessor room-guard-col
              :initarg :guard-col)
   (direction :initform :north
              :initarg :direction
              :accessor guard-direction)))

(defun make-room-state (&optional to-copy)
  (if to-copy
      (make-instance 'room-state
                     :input-map (room-input-map to-copy)
                     :num-rows (room-rows to-copy)
                     :num-cols (room-cols to-copy)
                     :guard-row (room-guard-row to-copy)
                     :guard-col (room-guard-col to-copy)
                     :direction (guard-direction to-copy))
      (let* (guard-row
             guard-col
             (input (cleaned-input))
             (total-rows (first (array-dimensions input)))
             (total-cols (second (array-dimensions input))))
        (dotimes (r total-rows)
          (dotimes (c total-cols)
            (when (eq (aref input r c) #\^)
              (setf guard-row r
                    guard-col c))))

        (make-instance 'room-state
                       :input-map (cleaned-input)
                       :num-rows (first (array-dimensions input))
                       :num-cols (second (array-dimensions input))
                       :guard-row guard-row
                       :guard-col guard-col
                       :direction :north))))

(defmethod at ((obj room-state) row col)
  (with-slots (floor-map)
      obj
    (when (array-in-bounds-p floor-map row col)
      (case (aref floor-map row col)
        (#\# :obstacle)
        (#\. :empty)
        (#\^ :guard)))))

(defmethod current-location ((obj room-state))
  (with-slots (guard-row guard-col direction)
      obj
    (list guard-row guard-col direction)))

(defmethod turn ((obj room-state)) 
  (with-slots (direction)
      obj
    (setf direction (ccase direction
               (:north :east)
                           (:east :south)
                           (:south :west)
                           (:west :north)))
    (current-location obj)))

(defmethod move ((obj room-state))
  (read-char)
  (with-slots (num-rows num-cols guard-row guard-col direction)
      obj
    (case direction
      (:north (cond
                ((zerop guard-row)
                 :exit)
                ((eq (at obj (- guard-row 1) guard-col) :obstacle)
                 :blocked)
                (t (decf guard-row)
                   (current-location obj))))
      (:east (cond
               ((= guard-col (- num-cols 1))
                :exit)
               ((eq (at obj guard-row (1+ guard-col)) :obstacle)
                :blocked)
               (t (incf guard-col)
                  (current-location obj))))
      (:south (cond
                ((= guard-row (- num-rows 1))
                 :exit)
                ((eq (at obj (+ guard-row 1) guard-col) :obstacle)
                 :blocked)
                (t (incf guard-row)
                   (current-location obj))))
      (:west (cond
               ((zerop guard-col)
                :exit)
               ((eq (at obj guard-row (- guard-col 1)) :obstacle)
                :blocked)
               (t (decf guard-col)
                  (current-location obj)))))))

(defmethod guard-step ((obj room-state) &optional spots-visited)
  (let ((result (move obj)))
    (case result
      (:blocked (turn obj))
      ((and spots-visited
            (member result spots-visited :test #'equal))
       :loop)
      (t result))))

(defun guard-visited-spots (state)
  (do* ((spots-visited (list (list (room-guard-row state) (room-guard-col state))))
        (result (guard-step state) (guard-step state)))
      ((eq result :exit) spots-visited)
    (when (consp result)
      (pushnew (list (first result) (second result))
               spots-visited :test #'equal))))

(defun obstruction-causes-loop (state pos)
  (when (not (equal pos (list (room-guard-row state))))
    (let ((state-copy (make-room-state state)))
      (setf (aref (room-input-map state-copy) (first pos) (second pos)) #\#)
      (do* ((spots-visited (list (list (room-guard-row state-copy) (room-guard-col state-copy))))
            (result (guard-step state spots-visited)
                    (guard-step state spots-visited)))
           ((or (eq result :exit)
                (eq result :loop))
            (eq result :loop))
        (when (consp result)
          (pushnew (list (first result) (second result))
                   spots-visited :test #'equal))))))

(defun debug-log (state &optional visited)
  (labels ((spot-at (r c &optional visited)
             ))
    (format t
            "~a~a~a~%~a~a~a~%~a~a~a"
            ())))

(defun part-1 ()
  (let* ((state (make-room-state))
         (spots-visited (guard-visited-spots state)))
    (length spots-visited)))

(defun part-2 ()
  (let* ((root-state (make-room-state))
         (root-path (guard-visited-spots (make-room-state root-state)))
         (loop-count 0))
    (loop for pos in root-path do
      (when (obstruction-causes-loop root-state pos)
        (incf loop-count)))
    loop-count))
