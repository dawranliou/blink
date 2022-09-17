(in-package #:blink)

(defclass scene ()
  ((last-tick-time :accessor last-tick-time :initform nil)
   (w :accessor w :initarg :w)
   (h :accessor h :initarg :h)
   (resources :accessor resources :initform (make-hash-table))
   (entities :accessor entities :initform ())
   (dt :accessor dt :initform 0)
   (camera :accessor camera :initarg :camera)
   (pausedp :accessor pausedp :initform nil)
   ;; Quick menu
   (pause-menu-selected :accessor pause-menu-selected :initform 0)
   (pause-menu-items :initarg :pause-menu-items :accessor pause-menu-items)
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

(defmethod update :around ((scene scene) &key keys keys-prev &allow-other-keys)
  (when (quit-confirmed-p scene)
    (unload scene)
    (funcall (on-quitting scene)))
  (with-slots (dt last-tick-time) scene
    (let ((current-tick-time (sdl2:get-ticks)))
      (unless last-tick-time
        (setf last-tick-time current-tick-time))
      (setf dt (- current-tick-time last-tick-time)
            last-tick-time current-tick-time)))
  (when (and (not (gethash "Escape" keys-prev))
             (gethash "Escape" keys))
    (setf (pausedp scene) (not (pausedp scene))))
  (if (pausedp scene)
      (cond
        ((and (gethash "Up" keys)
              (not (gethash "Up" keys-prev)))
         (setf (pause-menu-selected scene)
               (clamp 0
                      (1- (pause-menu-selected scene))
                      (length (pause-menu-items scene)))))
        ((and (gethash "Down" keys)
              (not (gethash "Down" keys-prev)))
         (setf (pause-menu-selected scene)
               (clamp 0
                      (1+ (pause-menu-selected scene))
                      (1- (length (pause-menu-items scene)))))))
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
    ;; Background blurring overlay
    (sdl2:set-render-draw-color renderer 0 0 0 100)
    (sdl2:render-fill-rect renderer (sdl2:make-rect 0 0 (w scene) (h scene)))

    ;; Dialog box
    (sdl2:set-render-draw-color renderer 0 0 0 200)
    (sdl2:render-fill-rect renderer
                           (sdl2:make-rect 100 100
                                           (- (w scene) 200)
                                           (- (h scene) 200)))
    (sdl2:set-render-draw-color renderer 255 255 255 255)
    (sdl2:render-draw-rect renderer (sdl2:make-rect 100
                                                    100
                                                    (- (w scene) 200)
                                                    (- (h scene) 200)))
    (text renderer ">"
          125 (+ 150 (* (pause-menu-selected scene) 50))
          :resource-pool (resources scene))
    (loop :for menu-item :in (pause-menu-items scene)
          :for y = 150 :then (incf y 50)
          :for dest-rect = (sdl2:make-rect 150 y
                                           (sdl2:texture-width menu-item)
                                           (sdl2:texture-height menu-item))
          :do (sdl2:render-copy renderer
                                menu-item
                                :source-rect (cffi:null-pointer)
                                :dest-rect dest-rect))))
