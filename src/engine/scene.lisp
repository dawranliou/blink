(in-package #:blink)

(defvar *scene* nil)
(defvar *scene-prev* nil)

(defun set-scene (scene &key renderer)
  (setf *scene-prev* *scene*)
  (unload *scene*)
  (setf *scene* scene)
  (init *scene* :renderer renderer))

(defclass scene ()
  ((last-tick-time :accessor last-tick-time :initform (sdl2:get-ticks))
   (dt :accessor dt :initform 0)))

(defgeneric init (scene &key &allow-other-keys))

(defmethod init (obj &key &allow-other-keys))

(defgeneric update (scene &key &allow-other-keys))

(defmethod update (obj &key &allow-other-keys))

(defmethod update :before ((scene scene) &key &allow-other-keys)
  (with-slots (dt last-tick-time) scene
    (let ((current-tick-time (sdl2:get-ticks)))
      (setf dt (- current-tick-time last-tick-time))
      (setf last-tick-time current-tick-time))))

(defgeneric render (scene &key &allow-other-keys))

(defmethod render (obj &key &allow-other-keys))

(defgeneric unload (scene &key &allow-other-keys))

(defmethod unload (obj &key &allow-other-keys))