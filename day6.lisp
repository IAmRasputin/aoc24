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

(defun make-room-state (&optional (input (cleaned-input)))
  (let (guard-row guard-col
         (total-rows (first (array-dimensions input)))
         (total-cols (second (array-dimensions input))))

    (dotimes (r total-rows)
      (dotimes (c total-cols)
        (when (eq (aref input r c) #\^)
          (setf guard-row r
                guard-col c))))

    (make-instance 'room-state
                   :input-map input
                   :num-rows (first (array-dimensions input))
                   :num-cols (second (array-dimensions input))
                   :guard-row guard-row
                   :guard-col guard-col
                   :direction :north)))

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
  (with-slots (num-rows num-cols guard-row guard-col direction)
      obj
    (case direction
      (:north (cond
                ((zerop guard-row) :exit)
                ((eq (at obj (- guard-row 1) guard-col) :obstacle) :blocked)
                (t (decf guard-row)
                   (current-location obj))))
      (:east (cond
               ((= guard-col (- num-cols 1)) :exit)
               ((eq (at obj guard-row (1+ guard-col)) :obstacle) :blocked)
               (t (incf guard-col)
                  (current-location obj))))
      (:south (cond
                ((= guard-row (- num-rows 1)) :exit)
                ((eq (at obj (+ guard-row 1) guard-col) :obstacle) :blocked)
                (t (incf guard-row)
                   (current-location obj))))
      (:west (cond
               ((zerop guard-col) :exit)
               ((eq (at obj guard-row (- guard-col 1)) :obstacle) :blocked)
               (t (decf guard-col)
                  (current-location obj)))))))

(defmethod guard-step ((obj room-state))
  (let ((result (move obj)))
    (case result
      (:blocked (turn obj))
      (t result))))

(defun guard-visited-spots (state)
  (do* ((spots-visited (list (list (room-guard-row state) (room-guard-col state))))
        (result (guard-step state) (guard-step state)))
      ((eq result :exit) spots-visited)
    (when (consp result)
      (pushnew (list (first result) (second result)) spots-visited :test #'equal))))

(defun part-1 ()
  (let* ((state (make-room-state))
         (spots-visited (guard-visited-spots state)))
    (length spots-visited)))

(defun obstruction-causes-loop (state row col)
  (unless (and (= row (room-guard-row state))
               (= col (room-guard-col state)))
    (let ((raw-input (cleaned-input)))
      (setf (aref raw-input row col) #\#)
      (do* ((state (make-room-state raw-input))
            (spots-visited (list (current-location state)))
            (result (guard-step state) (guard-step state))
            (is-loop (member result spots-visited :test #'equal)
                     (member result spots-visited :test #'equal)))
          ((or is-loop (eq result :exit)) is-loop)
        (pushnew result spots-visited :test #'equal)))))
 
(defun part-2 ()
  (let ((floor-map (make-room-state))
        (loop-count 0))
    (loop for r below (room-rows floor-map) do
          (loop for c below (room-cols floor-map) do
                (if (obstruction-causes-loop floor-map r c)
                    (progn
                      (incf loop-count)
                      loop-count))))
    loop-count))
