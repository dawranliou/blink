(in-package #:blink)

(defun make-player (tex x y)
  (make-entity
   (list (make-box-component x y
                             (* 2 +sprite-size+)
                             (* 2 +sprite-size+)
                             +white+)
         (make-sprite-component tex
                                (sdl2:make-rect 0 48 16 16)
                                x y
                                (* 2 +sprite-size+)
                                (* 2 +sprite-size+)))))
