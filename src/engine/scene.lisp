(in-package #:blink)

(defvar *scene* nil)
(defvar *scene-prev* nil)

(defun set-scene (scene &key renderer)
  (setf *scene-prev* *scene*)
  (unload *scene*)
  (setf *scene* scene)
  (init *scene* :renderer renderer))

(defclass scene () ())

(defgeneric init (scene &key &allow-other-keys))

(defmethod init (obj &key &allow-other-keys))

(defgeneric update (scene dt &key &allow-other-keys))

(defmethod update (obj dt &key &allow-other-keys))

(defgeneric render (scene &key &allow-other-keys))

(defmethod render (obj &key &allow-other-keys))

(defgeneric unload (scene &key &allow-other-keys))

(defmethod unload (obj &key &allow-other-keys))
