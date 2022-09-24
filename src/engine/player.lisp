(in-package #:blink)

(defparameter +player-speed+ 16)

(defclass player (sprite velocity animator)
  ((groundedp :accessor groundedp :initform nil)))

(defun make-player (tex x y &key (current-animation :idle))
  (let ((player (make-instance 'player
                               :tex tex
                               :rect (sdl2:make-rect 3 3 9 13)
                               :x x :y y
                               :h 52
                               :w 36
                               :current-animation current-animation)))
    (with-slots (animations) player
      (setf (gethash :idle animations)
            (list (sdl2:make-rect 3 3 9 13)
                  (sdl2:make-rect 3 3 9 13)
                  (sdl2:make-rect 19 3 9 13)
                  (sdl2:make-rect 19 3 9 13)
                  (sdl2:make-rect 3 3 9 13)
                  (sdl2:make-rect 3 3 9 13)
                  (sdl2:make-rect 19 3 9 13)
                  (sdl2:make-rect 19 3 9 13)))
      (setf (gethash :run animations)
            (list (sdl2:make-rect 3 19 9 13)
                  (sdl2:make-rect 19 19 9 13)
                  (sdl2:make-rect 3 19 9 13)
                  (sdl2:make-rect 19 19 9 13)
                  (sdl2:make-rect 3 19 9 13)
                  (sdl2:make-rect 19 19 9 13)
                  (sdl2:make-rect 3 19 9 13)
                  (sdl2:make-rect 19 19 9 13))))
    player))

(defun player-move (player &key (x 0) (y 0))
  (incf (x player) (floor x))
  (incf (y player) (floor y)))

(defun player-teleport (player &key x y)
  (when x
    (setf (x player) x))
  (when y
    (setf (y player) y)))
