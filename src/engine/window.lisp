(in-package #:blink)

(defclass game-window (kit.sdl2:window)
  ((renderer :initform nil :reader renderer)
   (frames :initform 0 :accessor frames)
   (scene :accessor scene)
   ;; Scene transitioning
   (transp :accessor transp :initform nil)
   (trans-alpha :accessor trans-alpha :initform 0.0)
   (trans-fade-out-p :accessor trans-fade-out-p :initform nil)
   (trans-to-scene :accessor trans-to-scene :initform nil)
   (keys :accessor keys :initform (make-hash-table :test 'equal))))

(defun transition-to-scene (game-window scene &key (alpha 0.0))
  (setf (transp game-window) t
        (trans-alpha game-window) alpha
        (trans-to-scene game-window) scene
        (trans-fade-out-p game-window) nil))

(defmethod kit.sdl2:initialize-window progn
    ((window game-window) &key init-scene &allow-other-keys)
  (sdl2-ttf:init)
  (setf (kit.sdl2:idle-render window) t)
  (sdl2:set-hint :render-scale-quality nil)
  (with-slots (renderer scene kit.sdl2:sdl-window) window
    (setf renderer (sdl2:create-renderer kit.sdl2:sdl-window
                                         nil
                                         '(:accelerated
                                           :presentvsync)))
    (sdl2:set-render-draw-blend-mode renderer :blend)
    (transition-to-scene window init-scene :alpha 1.0)))

(defmethod kit.sdl2:close-window :before ((window game-window))
  (remove-all-entities-from-scene (scene window))
  (with-slots (renderer) window
    (when (and (slot-boundp window 'renderer) renderer)
      (sdl2:destroy-renderer renderer)))
  (loop for resource being the hash-values of *resources*
        do (free-resource resource))
  (clrhash *resources*))

(defmethod kit.sdl2:close-window :after ((window game-window))
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
            (set-scene trans-to-scene :renderer renderer)
            (setf scene trans-to-scene
                  trans-fade-out-p t))))))

(defmethod kit.sdl2:render ((window game-window))
  (with-slots (frames) window
    (incf frames)
    (when (< 6000 frames)
      (setf frames 0)))
  (when (transp window)
    (update-transition window))
  (with-slots (frames renderer scene keys) window
    (sdl2:set-render-draw-color renderer 0 0 0 255)
    (sdl2:render-clear renderer)
    (update scene :keys keys)
    (render renderer scene)))

(defun game-window-rect (window)
  (multiple-value-bind (w h) (kit.sdl2:window-size window)
    (sdl2:make-rect 0 0 w h)))

(defmethod kit.sdl2:render :after ((window game-window))
  (with-slots (renderer transp trans-alpha frames) window
    ;; (text renderer (format nil "~A" frames) 10 10)
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

(defmethod kit.sdl2:textinput-event :after ((window game-window) ts text)
  (when (string= "Q" (string-upcase text))
    (kit.sdl2:close-window window)))

(defmethod kit.sdl2:keyboard-event
    ((window game-window) state ts repeat-p keysym)
  (with-slots (scene keys) window
    (let ((scancode (sdl2:scancode keysym)))
      (unless repeat-p
        ;; (format t "~A ~S ~S~%" state scancode (sdl2:scancode-name scancode))
        (case state
          (:keyup (setf (gethash (sdl2:scancode-name scancode) keys) nil))
          (:keydown (setf (gethash (sdl2:scancode-name scancode) keys) t)))))))

(defmethod kit.sdl2:keyboard-event :after
    ((window game-window) state ts repeat-p keysym)
  (let ((scancode (sdl2:scancode keysym)))
    (when (eq :scancode-escape scancode)
      (kit.sdl2:close-window window))))
