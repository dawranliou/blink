(in-package #:blink)

(defparameter +width+ (* +sprite-size+ 20))
(defparameter +height+ (* +sprite-size+ 18))

(defclass game-window (kit.sdl2:window)
  ((renderer :initform nil :reader renderer)
   (scene :accessor scene)
   (resources :accessor resources :initform (make-hash-table))
   ;; Scene transitioning
   (transp :accessor transp :initform nil)
   (trans-alpha :accessor trans-alpha :initform 0.0)
   (trans-fade-out-p :accessor trans-fade-out-p :initform nil)
   (trans-to-scene :accessor trans-to-scene :initform nil)
   (keys-prev :accessor keys-prev :initform (make-hash-table :test 'equal))
   (keys :accessor keys :initform (make-hash-table :test 'equal))))

(defun transition-to-scene (game-window scene &key (alpha 0.0))
  (setf (transp game-window) t
        (trans-alpha game-window) alpha
        (trans-to-scene game-window) scene
        (trans-fade-out-p game-window) nil))

(defmethod initialize-instance :after
    ((window game-window) &key init-scene &allow-other-keys)
  (sdl2-ttf:init)
  (setf (kit.sdl2:idle-render window) t)
  (sdl2:set-hint :render-scale-quality nil)
  (with-slots (renderer scene resources kit.sdl2:sdl-window) window
    (setf renderer (sdl2:create-renderer kit.sdl2:sdl-window
                                         nil
                                         '(:accelerated
                                           :presentvsync)))
    (sdl2:set-render-draw-blend-mode renderer :blend)
    (setf (scene window) init-scene)
    (with-resource-pool resources
      (init init-scene))
    (transition-to-scene window init-scene :alpha 1.0)))

(defmethod kit.sdl2:close-window :before ((window game-window))
  (setf *default-font* nil)
  (with-slots (renderer) window
    (when (and (slot-boundp window 'renderer) renderer)
      (sdl2:destroy-renderer renderer))))

(defmethod kit.sdl2:close-window :after ((window game-window))
  (with-resource-pool (resources window)
    (free-all-resources))
  (sdl2-ttf:quit)
  (kit.sdl2:quit))

(defmethod kit.sdl2::additional-window-flags append ((window game-window))
  '(:shown))

(defun update-transition (game-window)
  (with-slots (trans-fade-out-p
               trans-alpha
               scene
               trans-to-scene
               transp
               resources
               renderer)
      game-window
    (if trans-fade-out-p
        (progn
          (incf trans-alpha -0.02)
          (when (<= trans-alpha -0.01)
            (setf trans-alpha 0
                  trans-fade-out-p nil
                  transp nil
                  trans-to-scene nil)))
        (progn
          (incf trans-alpha 0.05)
          (when (< 1.01 trans-alpha)
            (with-resource-pool resources
              (unload scene)
              (init trans-to-scene))
            (setf scene trans-to-scene
                  trans-fade-out-p t))))))

(defmethod kit.sdl2:render ((window game-window))
  (with-slots (renderer resources scene keys keys-prev) window
    (with-renderer renderer
      (with-resource-pool resources
        (sdl2:set-window-title (kit.sdl2:sdl-window window)
                               (format nil "FPS ~A" (last-fps scene)))

        (when (transp window)
          (update-transition window))
        (sdl2:set-render-draw-color renderer 0 0 0 255)
        (sdl2:render-clear renderer)
        (unless (transp window)
          (update scene :keys keys :keys-prev keys-prev))
        (render scene)
        ;; Record current rendering cycle's keys state to the prev key state table
        (loop :for k
                :being :the :hash-key
                  :using (hash-value v) :of keys
              :do (setf (gethash k keys-prev) v))))))

(defun game-window-rect (window)
  (multiple-value-bind (w h) (kit.sdl2:window-size window)
    (sdl2:make-rect 0 0 w h)))

(defmethod kit.sdl2:render :after ((window game-window))
  (with-slots (renderer transp trans-alpha) window
    (when transp
      (let ((alpha (floor (* 255 (cond
                                   ((< trans-alpha 0) 0)
                                   ((< 1 trans-alpha) 1)
                                   (t trans-alpha))))))
        ;; (format t "~A~%" alpha)
        (sdl2:set-render-draw-color renderer 0 0 0 alpha)
        (sdl2:render-fill-rect renderer (game-window-rect window))))
    ;; (sdl2:set-render-draw-color renderer 0 0 0 100)
    ;; (sdl2:render-fill-rect renderer (game-window-rect window))
    (sdl2:render-present renderer)))

(defmethod kit.sdl2:render :around ((window game-window))
  (if (quit-confirmed-p (scene window))
      (kit.sdl2:close-window window)
      (call-next-method)))

(defmethod kit.sdl2:keyboard-event
    ((window game-window) state ts repeat-p keysym)
  (with-slots (scene keys keys-prev) window
    (let ((key-name (sdl2:scancode-name (sdl2:scancode keysym))))
      ;; (format t "~A ~S ~S~%" state key-name repeat-p)
      (unless repeat-p
        (case state
          (:keyup (setf (gethash key-name keys) nil))
          (:keydown (setf (gethash key-name keys) t)))))))
