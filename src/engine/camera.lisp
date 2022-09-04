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

(defclass lerp-camera (camera)
  ((lerp-a :accessor lerp-a :initform (floor +width+ 2))
   (lerp-b :accessor lerp-b :initform (floor +height+ 2))
   (lerp-t :accessor lerp-t :initform 0.05)))

(defmethod (setf x) (new-val (camera lerp-camera))
  (with-slots (lerp-a lerp-t) camera
    (setf (slot-value camera 'x)
          (floor (min lerp-a
                      (lerp (x camera) (- lerp-a new-val) lerp-t))))))

(defmethod (setf y) (new-val (camera lerp-camera))
  (with-slots (lerp-b lerp-t) camera
    (setf (slot-value camera 'y)
          (floor (min lerp-b
                      (lerp (y camera) (- lerp-b new-val) lerp-t))))))

(defun lerp (a b time)
  (+ (* (- 1 time) a) (* time b)))
