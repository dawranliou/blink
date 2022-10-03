(in-package #:blink)

(defclass scene ()
  ((last-tick-time :accessor last-tick-time :initform nil)
   (w :accessor w :initarg :w)
   (h :accessor h :initarg :h)
   (entities :accessor entities :initform ())
   (dt :accessor dt :initform 0)
   (frames :initform 0 :accessor frames)
   (frame-timer :accessor frame-timer :initform 1.0)
   (last-fps :accessor last-fps :initform 0)
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
  (with-slots (dt frames frame-timer last-fps last-tick-time) scene
    (let ((current-tick-time (sdl2:get-ticks)))
      (unless last-tick-time
        (setf last-tick-time current-tick-time))
      (setf dt (- current-tick-time last-tick-time)
            last-tick-time current-tick-time))
    (incf frames)
    (incf frame-timer dt)
    (when (< 1000 frame-timer)
      (decf frame-timer 1000)
      (setf last-fps frames
            frames 0)))
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

;;; Level editor
(defclass level-editor-scene (scene)
  ((spritesheet :initarg :spritesheet :accessor spritesheet)
   (tiles :initarg :level :accessor tiles)
   (current-tile :initform 0 :accessor current-tile)
   (draw-grid-p :initform t :accessor draw-grid-p)
   (mouse :accessor mouse))
  (:default-initargs
   :w +width+
   :h +height+))

(defmethod init ((scene level-editor-scene) &key)
  (setf (camera scene)
        (make-bounded-camera (* 30 +sprite-size+)
                             (* 30 +sprite-size+)
                             +width+
                             +height+))
  (with-slots (spritesheet) scene
    (unless (typep spritesheet 'tex)
      (setf spritesheet (load-resource spritesheet :type :image)))))

(defmethod render ((scene level-editor-scene) &key)
  ;; Left viewport
  (render-set-viewport (sdl2:make-rect 0 0 +width+ +height+))
  (run-renderer-system (entities scene)
                       :camera (camera scene)
                       :w (w scene)
                       :h (h scene))

  ;; Right viewport
  (render-set-viewport (sdl2:make-rect +width+ 0 300 +height+))
  (render-draw-rect (sdl2:make-rect 0 0 300 +height+) :color +white+)

  (text (format nil "TILE ~A" (current-tile scene)) 0 10)
  (render-copy (texture (spritesheet scene))
               (sdl2:make-rect 0 50
                               (* 2 (w (spritesheet scene)))
                               (* 2 (h (spritesheet scene)))))
  (render-draw-rect (sdl2:make-rect (mod (* +sprite-size+ (current-tile scene))
                                         (* 2 (w (spritesheet scene))))
                                    (+ 50
                                       (floor (* +sprite-size+ (current-tile scene))
                                              (* 2 (h (spritesheet scene)))))
                                    +sprite-size+
                                    +sprite-size+)
                    :color (rgb 255 0 0))

  )

(defmethod update ((scene level-editor-scene) &key keys mouse-pos)
  (when (gethash "Left" keys)
    (decf (x (camera scene)) 10))
  (when (gethash "Right" keys)
    (incf (x (camera scene)) 10))

  (when (gethash "Up" keys)
    (decf (y (camera scene)) 10))
  (when (gethash "Down" keys)
    (incf (y (camera scene)) 10))

  (setf (mouse scene) mouse-pos)
  )
