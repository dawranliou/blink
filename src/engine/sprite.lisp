(in-package #:blink)

(defvar *debug-sprite* nil
  "Global debug flag for checking sprite's bounding box.")

;; (setf *debug-sprite* t)
;; (setf *debug-sprite* nil)

(defparameter +sprite-size+ 32)

(defclass sprite (entity rect color texture) ())

(defun make-sprite-rect (x y)
  (make-instance 'rect :x x :y y :w +sprite-size+ :h +sprite-size+))

(defun make-sprite (tex rect x y w h &key color)
  (make-instance 'sprite :tex tex :rect rect :x x :y y :w w :h h :color color))

(defmethod render ((sprite sprite) &key camera)
  (with-slots (tex rect flip x y w h color) sprite
    (let ((dest-rect (if camera
                         (sdl2:make-rect (- x (x camera)) (- y (y camera)) w h)
                         (sdl2:make-rect x y w h))))
      (when *debug-sprite*
        (render-draw-rect dest-rect :color (or color +gray-50+)))
      (when color
        (destructuring-bind (r g b _a) color
          (declare (ignore _a))
          (sdl2:set-texture-color-mod (texture tex) r g b)))
      (render-copy (texture tex) dest-rect :src-rect rect :flip flip))))
