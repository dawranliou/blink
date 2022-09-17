(in-package #:blink)

(defvar *scene* nil)
(defvar *scene-prev* nil)

(defun set-scene (scene &key renderer)
  (setf *scene-prev* *scene*)
  (when *scene*
    (unload *scene*))
  (setf *scene* scene)
  (init *scene* :renderer renderer))

(defclass scene ()
  ((last-tick-time :accessor last-tick-time :initform nil)
   (w :accessor w :initarg :w)
   (h :accessor h :initarg :h)
   (resources :accessor resources :initform (make-hash-table))
   (entities :accessor entities :initform ())
   (dt :accessor dt :initform 0)
   (camera :accessor camera :initarg :camera)
   (pausedp :accessor pausedp :initform nil)
   (quit-confirmed-p :accessor quit-confirmed-p :initform nil)
   (on-quitting :accessor on-quitting :initarg :on-quitting)))

(defun add-to-scene (scene entity)
  (push entity (entities scene)))

(defun remove-entity-from-scene (scene entity)
  (setf (entities scene) (remove entity (entities scene))))

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
      (unless last-tick-time
        (setf last-tick-time current-tick-time))
      (setf dt (- current-tick-time last-tick-time)
            last-tick-time current-tick-time))))

(defmethod update :around ((scene scene) &key &allow-other-keys)
  (when (quit-confirmed-p scene)
    (unload scene)
    (funcall (on-quitting scene)))
  (unless (pausedp scene)
    (call-next-method)))

(defgeneric unload (scene &key &allow-other-keys))

(defmethod unload ((scene scene) &key &allow-other-keys)
  (remove-all-entities-from-scene scene)
  (loop for resource being the hash-values of (resources scene)
        do (free-resource resource))
  (clrhash (resources scene)))

(defmethod render (renderer (scene scene) &key &allow-other-keys)
  (run-renderer-system renderer (entities scene) :camera (camera scene)))

(defmethod render :after (renderer (scene scene) &key &allow-other-keys)
  (when (pausedp scene)
    (sdl2:set-render-draw-color renderer 0 0 0 100)
    (sdl2:render-fill-rect renderer (sdl2:make-rect 0 0 (w scene) (h scene)))))
