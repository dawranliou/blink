(in-package #:blink)

(defvar *camera*)

(defclass camera ()
  ((x :accessor x :initarg :x)
   (y :accessor y :initarg :y)))
