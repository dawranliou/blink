(in-package #:blink)

(defparameter +player-speed+ 16)

(defclass player (sprite velocity animator)
  ((groundedp :accessor groundedp :initform nil)))

(defun make-player (tex x y)
  (let ((player (make-instance 'player
                               :tex tex
                               :rect (sdl2:make-rect 0 0 16 16)
                               :x x :y y
                               :h 64
                               :w 64
                               :current-animation :idle)))
    (with-slots (animations) player
      (setf (gethash :idle animations)
            (list (sdl2:make-rect 0 0 16 16)
                  (sdl2:make-rect 0 0 16 16)
                  (sdl2:make-rect 16 0 16 16)
                  (sdl2:make-rect 16 0 16 16)
                  (sdl2:make-rect 0 0 16 16)
                  (sdl2:make-rect 0 0 16 16)
                  (sdl2:make-rect 16 0 16 16)
                  (sdl2:make-rect 16 0 16 16)))
      (setf (gethash :run animations)
            (list (sdl2:make-rect 0 16 16 16)
                  (sdl2:make-rect 16 16 16 16)
                  (sdl2:make-rect 0 16 16 16)
                  (sdl2:make-rect 16 16 16 16)
                  (sdl2:make-rect 0 16 16 16)
                  (sdl2:make-rect 16 16 16 16)
                  (sdl2:make-rect 0 16 16 16)
                  (sdl2:make-rect 16 16 16 16))))
    player))

(defun player-move (player &key (x 0) (y 0))
  (incf (x player) (floor x))
  (incf (y player) (floor y)))

(defun player-teleport (player &key x y)
  (when x
    (setf (x player) x))
  (when y
    (setf (y player) y)))
