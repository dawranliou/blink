(in-package #:blink)

(defparameter +player-speed+ 16)

(defclass player (entity) ())

(defun make-player (tex x y)
  (make-entity
   'player
   (list
    (make-sprite-component tex
                           (sdl2:make-rect 0 48 16 16)
                           x y
                           (* 2 +sprite-size+)
                           (* 2 +sprite-size+)))))

(defun player-move (player &key (x 0) (y 0))
  (loop :for component :in (entity-components player)
        :when (typep component 'sprite)
          :do (progn
                (incf (x component) (floor x))
                (incf (y component) (floor y)))))

(defun player-pos (player)
  (with-slots (x y)
      (find-if (lambda (component) (typep component 'sprite))
               (entity-components player))
    (list x y)))
