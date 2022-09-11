(in-package #:blink)

(defclass interactable ()
  ((activep :accessor activep :initform nil)
   (active-sprite :initarg :active-sprite
                  :accessor active-sprite
                  :initform nil)))

(defun run-interactor-system (objects actor)
  (when objects
    (loop :for obj :in objects
          :when (typep obj 'interactable)
          :if (collidep obj actor)
            :do (setf (activep obj) t)
          :else :do (setf (activep obj) nil))))

(defmethod render :after (renderer (obj interactable) &key camera)
  (when (activep obj)
    (with-slots (x y active-sprite) obj
      (if active-sprite
          (render renderer active-sprite :camera camera)
          (let ((rect (sdl2:make-rect (- x (x camera) -16)
                                      (- y (y camera) 32)
                                      32
                                      32)))
            (apply #'sdl2:set-render-draw-color renderer +white+)
            (sdl2:render-draw-rect renderer rect))))))
