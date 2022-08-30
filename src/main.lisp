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
    (1 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 2)
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
  (add-room-to-scene level-scene *room*)

  (setf *player-tex* (load-texture-from-file
                      renderer
                      (relative-path #P"assets/player.png")))
  (setf *player* (make-player *player-tex* (* 4 +sprite-size+) (* 10 +sprite-size+)))
  (add-to-scene level-scene *player*))

(defmethod unload ((level-scene level-scene) &key &allow-other-keys)
  (setf *player-tex* nil)
  (setf *bg-tex* nil))

(defmethod update ((level-scene level-scene) &key keys &allow-other-keys)
  (with-slots (dt) level-scene
    (when (gethash "Right" keys)
      (player-move *player* :x (* dt +0.5)))

    (when (gethash "Left" keys)
      (player-move *player* :x (* dt -0.5)))

    (when (gethash "Up" keys)
      (player-move *player* :y (* dt -0.5)))

    (when (gethash "Down" keys)
      (player-move *player* :y (* dt +0.5)))))

(defmethod update :after ((level-scene level-scene) &key &allow-other-keys)
  (destructuring-bind (x y) (player-pos *player*)
    (set-scene-camera level-scene :x (- x (/ +width+ 2)) :y (- y (/ +height+ 2)))))

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
;; (load-room *room*)
;; (setf *player* (make-player *player-tex* (* 4 +sprite-size+) (* 10 +sprite-size+)))
;; (remove-all-entities-from-scene (scene *window*))
;; (setf (kit.sdl2:render-enabled *window*) t)
