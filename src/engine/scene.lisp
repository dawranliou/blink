(in-package #:blink)

(defclass scene ()
  ((last-tick-time :accessor last-tick-time :initform nil)
   (w :accessor w :initarg :w)
   (h :accessor h :initarg :h)
   (entities :accessor entities :initform ())
   (frames :initform 0 :accessor frames)
   (dt :accessor dt :initform 0)
   (camera :accessor camera :initarg :camera)
   (pausedp :accessor pausedp :initform nil)
   ;; Quick menu
   (pause-menu-selected :accessor pause-menu-selected :initform 0)
   (pause-menu-items :initarg :pause-menu-items :accessor pause-menu-items)
   (quit-confirmed-p :accessor quit-confirmed-p :initform nil)))

(defun add-to-scene (scene entity)
  (push entity (entities scene)))

(defun remove-entity-from-scene (scene entity)
  (setf (entities scene) (remove entity (entities scene))))

(defun remove-all-entities-from-scene (scene)
  (setf (entities scene) nil))

(defun center-scene-camera (scene &key x y)
  (when (or x y)
    (center-camera (camera scene) :x x :y y)))

(defgeneric init (scene &key &allow-other-keys))

(defmethod init (obj &key &allow-other-keys))

(defgeneric update (scene &key &allow-other-keys))

(defmethod update (obj &key &allow-other-keys))

(defmethod update :around ((scene scene) &key keys keys-prev &allow-other-keys)
  (with-slots (dt frames last-tick-time) scene
    (incf frames)
    (when (< 6000 frames)
      (setf frames 0))
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
                      (1- (length (pause-menu-items scene))))))
        ((and (gethash "X" keys)
              (not (gethash "X" keys-prev)))
         (cond
           ((= 0 (pause-menu-selected scene))
            (setf (pausedp scene) nil))
           ((= 1 (pause-menu-selected scene))
            (setf (quit-confirmed-p scene) t)))))
      (call-next-method)))

(defgeneric unload (scene &key &allow-other-keys))

(defmethod unload ((scene scene) &key &allow-other-keys)
  (remove-all-entities-from-scene scene))

(defmethod render ((scene scene) &key &allow-other-keys)
  (run-renderer-system (entities scene)
                       :camera (camera scene)
                       :w (w scene)
                       :h (h scene)))

(defmethod render :after ((scene scene) &key &allow-other-keys)
  (when (pausedp scene)
    ;; Background blurring overlay
    (render-fill-rect (sdl2:make-rect 0 0 (w scene) (h scene))
                      :color '(0 0 0 100))

    ;; Dialog box
    (render-fill-rect (sdl2:make-rect 100 100 (- (w scene) 200) (- (h scene) 200))
                      :color '(0 0 0 200))

    (text ">" 125 (+ 150 (* (pause-menu-selected scene) 50)))
    (loop :for menu-item :in (pause-menu-items scene)
          :for y = 150 :then (incf y 50)
          :do (text menu-item 150 y))))
