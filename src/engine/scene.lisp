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
   (entities :accessor entities :initform '())
   (dt :accessor dt :initform 0)
   (camera :accessor camera
           :initarg :camera
           :initform (make-instance 'camera))))

(defun add-to-scene (scene entity)
  (push entity (entities scene)))

(defun remove-all-entities-from-scene (scene)
  (setf (entities scene) nil))

(defun set-scene-camera (scene &key x y)
  (when (or x y)
    (set-camera (camera scene) :x x :y y)))

(defun move-scene-camera (scene &key dx dy)
  (when (or dx dy)
    (move-camera (camera scene) :dx dx :dy dy)))

(defgeneric init (scene &key &allow-other-keys))

(defmethod init (obj &key &allow-other-keys))

(defgeneric update (scene &key &allow-other-keys))

(defmethod update (obj &key &allow-other-keys))

(defmethod update :before ((scene scene) &key &allow-other-keys)
  (with-slots (dt last-tick-time) scene
    (let ((current-tick-time (sdl2:get-ticks)))
      (setf dt (- current-tick-time last-tick-time))
      (setf last-tick-time current-tick-time))))

(defgeneric unload (scene &key &allow-other-keys))

(defmethod unload (obj &key &allow-other-keys))
