(in-package #:blink)

(defvar *renderer*)

(defmacro with-renderer (renderer &body body)
  `(let ((*renderer* ,renderer))
     ,@body))

(defgeneric render (thing &key camera &allow-other-keys)
  (:method (thing &key camera &allow-other-keys)
    nil))

(defun run-renderer-system (entities &key camera w h resource-pool)
  (loop :for entity :in entities
        :do (render entity :camera camera
                           :w w :h h
                           :resource-pool resource-pool)))

(defun create-texture-from-surface (surface)
  (sdl2:create-texture-from-surface *renderer* surface))

(defun render-copy (texture dest-rect &key
                                        (src-rect (cffi:null-pointer))
                                        (flip nil))
  (sdl2:render-copy-ex *renderer*
                       texture
                       :source-rect src-rect
                       :dest-rect dest-rect
                       :flip flip))

(defun render-draw-rect (dest-rect &key color)
  (when color
    (apply #'sdl2:set-render-draw-color *renderer* color))
  (sdl2:render-draw-rect *renderer* dest-rect))

(defun render-fill-rect (dest-rect &key color)
  (when color
    (apply #'sdl2:set-render-draw-color *renderer* color))
  (sdl2:render-fill-rect *renderer* dest-rect))
