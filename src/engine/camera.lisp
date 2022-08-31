(in-package #:blink)

(defvar *camera*)

(defclass camera ()
  ((x :accessor x :initarg :x :initform 0)
   (y :accessor y :initarg :y :initform 0)))

(defmethod (setf x) (new-val (camera camera))
  (setf (slot-value camera 'x) (floor new-val)))

(defmethod (setf y) (new-val (camera camera))
  (setf (slot-value camera 'y) (floor new-val)))

(defun set-camera (camera &key x y)
  (when x
    (setf (x camera) x))
  (when y
    (setf (y camera) y)))

(defun move-camera (camera &key dx dy)
  (when dx
    (incf (x camera) dx))
  (when dy
    (incf (y camera) dy)))
