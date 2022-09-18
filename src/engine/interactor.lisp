(in-package #:blink)

(defclass interactable ()
  ((activep :accessor activep :initform nil)
   (active-sprite :initarg :active-sprite
                  :accessor active-sprite
                  :initform nil)
   (interactp :accessor interactp :initform nil)
   (interact-sprite :initarg :interact-sprite
                    :accessor interact-sprite
                    :initform nil)))

(defun run-interactor-system (objects actor &key interactp &allow-other-keys)
  (when objects
    (loop :for obj :in objects
          :when (typep obj 'interactable)
          :if (collidep obj actor)
            :do (setf (activep obj) t)
                (when interactp
                  (setf (interactp obj) t))
          :else :do (setf (activep obj) nil
                          (interactp obj) nil))))

(defmethod render :after (renderer (obj interactable) &key camera)
  (when (activep obj)
    (with-slots (x y active-sprite interactp interact-sprite) obj
      (if active-sprite
          (render renderer
                  (if (and interactp interact-sprite)
                      interact-sprite
                      active-sprite)
                  :camera camera)
          (let ((rect (sdl2:make-rect (- x (x camera) -16)
                                      (- y (y camera) 32)
                                      32
                                      32)))
            (apply #'sdl2:set-render-draw-color renderer +white+)
            (sdl2:render-draw-rect renderer rect))))))

(defclass conversable (interactable)
  ((conversation-idx :accessor conversation-idx :initform nil)
   (conversations :accessor conversations :initform nil)))

(defmethod render :after (renderer (obj conversable) &key w resource-pool)
  (when (interactp obj)
    ;; Dialog box
    (sdl2:set-render-draw-color renderer 0 0 0 200)
    (sdl2:render-fill-rect renderer (sdl2:make-rect 20 20 (- w 40) 160))
    (sdl2:set-render-draw-color renderer 255 255 255 255)
    (sdl2:render-draw-rect renderer (sdl2:make-rect 20 20 (- w 40) 160))

    ;; Headshot
    ;; (sdl2:render-draw-rect renderer (sdl2:make-rect 40 40 120 120))
    (sdl2:render-copy-ex renderer
                         (texture (tex obj))
                         :source-rect (sdl2:make-rect 2 3 10 10)
                         :dest-rect (sdl2:make-rect 40 40 120 120)
                         :flip nil)

    (text renderer (nth (conversation-idx obj) (conversations obj)) 180 40
          :resource-pool resource-pool)))
