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

(defmethod render :after ((obj interactable) &key camera)
  (when (activep obj)
    (with-slots (x y active-sprite interactp interact-sprite) obj
      (if active-sprite
          (render (if (and interactp interact-sprite)
                      interact-sprite
                      active-sprite)
                  :camera camera)
          (let ((rect (sdl2:make-rect (- x (x camera) -16)
                                      (- y (y camera) 32)
                                      32
                                      32)))
            (render-draw-rect rect :color +white+))))))

(defclass conversable (interactable)
  ((conversation-idx :accessor conversation-idx :initform nil)
   (conversations :accessor conversations :initform nil)))

(defmethod render :after ((obj conversable) &key w)
  (when (interactp obj)
    ;; Dialog box
    (render-fill-rect (sdl2:make-rect 20 20 (- w 40) 160) :color (rgb 0 0 0 0.8))
    (render-draw-rect (sdl2:make-rect 20 20 (- w 40) 160) :color +white+)

    ;; Headshot
    ;; (sdl2:render-draw-rect renderer (sdl2:make-rect 40 40 120 120))
    (render-copy (texture (tex obj))
                 (sdl2:make-rect 40 40 120 120)
                 :src-rect (sdl2:make-rect 2 3 10 10))

    (text (nth (conversation-idx obj) (conversations obj)) 180 40)))
