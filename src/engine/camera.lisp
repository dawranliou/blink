(in-package #:blink)

(defvar *camera*)

(defclass camera ()
  ((x :accessor x :initarg :x :initform 0)
   (y :accessor y :initarg :y :initform 0)
   (w :accessor w :initarg :w)
   (h :accessor h :initarg :h)
   (min-x :accessor min-x :initarg :min-x :initform 0)
   (max-x :accessor max-x :initarg :max-x :initform nil)
   (min-y :accessor min-y :initarg :min-y :initform 0)
   (max-y :accessor max-y :initarg :max-y :initform nil)))

(defmethod (setf x) (new-val (camera camera))
  (setf (slot-value camera 'x) (floor new-val)))

(defmethod (setf y) (new-val (camera camera))
  (setf (slot-value camera 'y) (floor new-val)))

(defgeneric center-camera (camera &key x y))

(defun clamp (min v max)
  (cond
    ((<= v min) min)
    ((<= max v) max)
    (t v)))

(defun make-bounded-camera (total-x total-y screen-w screen-h)
  (make-instance 'camera
                 :w screen-w
                 :h screen-h
                 :min-x 0
                 :min-y 0
                 :max-x (- total-x screen-w)
                 :max-y (- total-y screen-h)))

(defmethod center-camera ((camera camera) &key x y)
  (with-slots (w h min-x max-x min-y max-y) camera
    (setf (x camera) (clamp min-x (- x (floor w 2)) max-x))
    (setf (y camera) (clamp min-y (- y (floor h 2)) max-y))))

(defclass lerp-camera (camera)
  ((a :initarg :a :initform (floor +width+ 2))
   (b :initarg :b :initform (floor +height+ 2))
   (c :initarg :c :initform 0.05)))

(defun make-lerp-camera (total-x total-y screen-w screen-h)
  (make-instance 'lerp-camera
                 :min-x (floor screen-w 2)
                 :min-y (floor screen-h 2)
                 :max-x (- total-x screen-w)
                 :max-y (- total-y screen-h)
                 :a (floor screen-w 2)
                 :b (floor screen-h 2)
                 :c 0.05))

(defmethod center-camera ((camera lerp-camera) &key x y)
  (with-slots (a b c (orig-x x) (orig-y y) min-x max-x min-y max-y)
      camera
    (setf (x camera) (floor (min a (lerp orig-x (- x a) c))))
    (setf (y camera) (floor (min b (lerp orig-y (- y b) c))))))

(defun lerp (a b c)
  (+ (* (- 1 c) a)
     (*      c  b)))
