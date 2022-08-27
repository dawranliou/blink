(in-package #:blink)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CONSTANTS

(defparameter +sprite-size+ 32)
(defparameter +width+ (* +sprite-size+ 20))
(defparameter +height+ (* +sprite-size+ 18))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GLOBAL STATES

(defvar *window* nil)
;; (defparameter *scene* (make-instance 'scene))
;; (defparameter *window* nil)
;; (defparameter *scene-views* '())

(defparameter *room*
  '((1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2)
    (2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1)
    (1 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 2)
    (2 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 1)
    (1 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 2)
    (2 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 1)
    (1 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 2)
    (2 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 1)
    (1 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 2)
    (2 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 1)
    (1 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 2)
    (2 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 1)
    (1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2)
    (2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1)
    (1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2)
    (2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1)
    (1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2)
    (2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1)))

(defvar *player-tex* nil)
(defvar *player* nil)
(defvar *bg-tex* nil)

(defclass level-scene (scene) ())

(defmethod init ((level-scene level-scene) &key renderer)
  (setf *bg-tex* (load-texture-from-file
                  renderer
                  (relative-path #P"assets/bg.png")))
  (setf *player-tex* (load-texture-from-file
                      renderer
                      (relative-path #P"assets/player.png")))
  (setf *player* (make-player *player-tex* (* 4 +sprite-size+) (* 10 +sprite-size+)))
  (load-room *room*))

(defmethod unload ((level-scene level-scene) &key &allow-other-keys)
  (setf *player-tex* nil)
  (setf *bg-tex* nil))

(defmethod update ((level-scene level-scene) dt &key key state)
  (when (eq state :keydown)
    (case key
      (:scancode-right (player-move *player* :x +5))
      (:scancode-left (player-move *player* :x -5))
      (:scancode-up (player-move *player* :y -5))
      (:scancode-down (player-move *player* :y +5))
      (otherwise nil))))

(defun run (&key (w +width+) (h +height+))
  (kit.sdl2:init)
  (sdl2:in-main-thread nil
    (setf *window* (make-instance 'game-window
                                  :title "Blink!"
                                  :w w :h h
                                  :init-scene (make-instance 'level-scene))))
  (kit.sdl2:start)
  *window*)

;; (run)
;; (destroy-entities)
;; (load-room *room*)
;; (setf *player* (make-player *player-tex* (* 4 +sprite-size+) (* 10 +sprite-size+)))
