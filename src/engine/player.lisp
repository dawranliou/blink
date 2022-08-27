(in-package #:blink)

(defparameter +player-speed+ 16)

(defclass player (entity) ())

(defun make-player (tex x y)
  (make-entity
   'player
   (list (make-box-component x y
                             (* 2 +sprite-size+)
                             (* 2 +sprite-size+)
                             +white+)
         (make-sprite-component tex
                                (sdl2:make-rect 0 48 16 16)
                                x y
                                (* 2 +sprite-size+)
                                (* 2 +sprite-size+)))))

(defun player-move (player &key (x 0) (y 0))
  (loop :for component :in (entity-components player)
        :when (or (typep component 'box) (typep component 'sprite))
          :do (progn
                (incf (x component) x)
                (incf (y component) y))))
