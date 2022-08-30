(in-package #:blink)

(defparameter +player-speed+ 16)

(defclass player (sprite) ())

(defun make-player (tex x y)
  (make-sprite tex
               (sdl2:make-rect 0 48 16 16)
               x y
               (* 2 +sprite-size+) (* 2 +sprite-size+)))

(defun player-move (player &key (x 0) (y 0))
  (incf (x player) (floor x))
  (incf (y player) (floor y)))

(defun player-pos (player)
  (list (x player) (y player)))
