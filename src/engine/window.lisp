(in-package #:blink)

(defclass game-window (kit.sdl2:window)
  ((renderer :initform nil :reader renderer)
   (frames :initform 0)
   (scene :accessor scene)
   (keys :accessor keys :initform (make-hash-table :test 'equal))))

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
    (setf scene init-scene)
    (set-scene scene :renderer renderer)))

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

(defmethod kit.sdl2:render :before ((window game-window))
  (with-slots (renderer scene keys) window
    (sdl2:set-render-draw-color renderer 0 0 0 255)
    (sdl2:render-clear renderer)
    (update scene :keys keys)))

(defmethod kit.sdl2:render ((window game-window))
  (with-slots (frames renderer scene) window
    ;; (text renderer "Hello" 10 10)
    (run-render-system renderer (entities scene) (camera scene))))

(defmethod kit.sdl2:render :after ((window game-window))
  (with-slots (renderer) window
    (sdl2:render-present renderer)))

(defmethod kit.sdl2:textinput-event :after ((window game-window) ts text)
  (when (string= "Q" (string-upcase text))
    (kit.sdl2:close-window window)))

(defmethod kit.sdl2:keyboard-event ((window game-window) state ts repeat-p keysym)
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
