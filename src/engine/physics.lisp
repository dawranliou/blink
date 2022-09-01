(in-package #:blink)

(defparameter +gravity+ 0.01)
(defparameter +max-speed+ 1)

(defun jump (entity)
  (setf (vy entity) -1))

(defun free-fall (entity dt)
  (let ((target-speed (+ (vy entity) (* dt +gravity+))))
    (setf (vy entity) (if (< target-speed +max-speed+)
                        target-speed
                        +max-speed+)))
  (incf (y entity) (floor (* dt (vy entity)))))

(defgeneric collidep (obj-a obj-b)
  (:method (obj-a obj-b)
    nil))

(defmethod collidep ((rect-a rect) (rect-b rect))
  (and (<= (x rect-a) (+ (x rect-b) (w rect-b)))
       (<= (x rect-b) (+ (x rect-a) (w rect-a)))
       (<= (y rect-a) (+ (y rect-b) (h rect-b)))
       (<= (y rect-b) (+ (Y rect-a) (h rect-a)))))

#|
(collidep (make-instance 'rect :x 0 :y 0 :w 10 :h 10)
          (make-instance 'rect :x 5 :y 5 :w 10 :h 10))
(collidep (make-instance 'rect :x 0 :y 0 :w 10 :h 10)
          (make-instance 'rect :x 11 :y 10 :w 10 :h 10))
|#
