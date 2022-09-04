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

(defparameter *tiles*
  '((1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1)
    (2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2)
    (1 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 1)
    (2 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 2)
    (1 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 1)
    (2 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 2)
    (1 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 1)
    (2 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 2)
    (1 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 2 1 2 1 2 1)
    (2 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 1 2 1 2 1 2)
    (1 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 2)
    (2 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 1)
    (1 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 2)
    (2 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 1)
    (1 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 2)
    (2 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 1)
    (1 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 2)
    (2 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 1)
    (1 2 0 0 0 0 0 0 0 0 0 0 1 2 1 2 0 0 1 2)
    (2 1 0 0 0 0 0 0 0 0 0 0 2 1 2 1 0 0 2 1)
    (1 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 2)
    (2 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 1)
    (1 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 2)
    (2 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 1)
    (1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2)
    (2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1)))

(defvar *player-tex* nil)
(defvar *player* nil)
(defvar *bg-tex* nil)

(defclass level-scene (scene) ()
  ;; (:default-initargs
  ;;  :camera (make-instance 'lerp-camera))
  )

(defmethod init ((level-scene level-scene) &key renderer)
  (setf *bg-tex* (load-texture-from-file
                  renderer
                  (relative-path #P"assets/bg.png")))
  (add-tiles-to-scene level-scene *tiles*)

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
    ;; update player state
    (with-slots (x y w h groundedp) *player*
      (setf groundedp
            (collide-with-tile *tiles* (make-rect x (+ y (* dt 0.5)) :w w :h h))))

    ;; update horizontal speed
    (cond
      ((gethash "Right" keys) (setf (vx *player*) +0.5))
      ((gethash "Left" keys) (setf (vx *player*) -0.5))
      (t (setf (vx *player*) 0)))

    ;; update vertical speed
    (if (groundedp *player*)
        (cond
          ((gethash "Space" keys) (jump *player*))
          (t (setf (vy *player*) 0)))
        (free-fall *player* dt))

    ;; collision detection
    (with-slots (x y vx vy w h) *player*
      (let ((target-x (+ x (floor (* dt vx))))
            (target-y (+ y (floor (* dt vy)))))
        ;; X collision
        (cond
          ;; Left
          ((or (solidp *tiles* target-x target-y)
               (solidp *tiles* target-x (+ target-y h -1)))
           (setf vx 0
                 x (* +sprite-size+ (floor (+ target-x w) +sprite-size+))))
          ;; Right
          ((or (solidp *tiles* (+ target-x w) target-y)
               (solidp *tiles* (+ target-x w) (+ target-y h -21)))
           (setf vx 0
                 x (* +sprite-size+ (floor target-x +sprite-size+))))
          (t (setf x target-x)))
        ;; Y collision (bottom)
        (if (or (solidp *tiles* x (+ target-y h))
                (solidp *tiles* (+ x w) (+ target-y h)))
            (setf vy 0
                  y (* +sprite-size+ (floor target-y +sprite-size+)))
            (progn (free-fall *player* dt)
                   (setf y target-y)))))))

(defmethod update :after ((level-scene level-scene) &key &allow-other-keys)
  (with-slots (x y) *player*
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
;; (add-tiles-to-scene (scene *window*) *tiles*)
;; (setf *player* (make-player *player-tex* (* 4 +sprite-size+) (* 10 +sprite-size+)))
;; (add-to-scene (scene *window*) *player*)
;; (remove-all-entities-from-scene (scene *window*))
;; (setf *debug* t)
;; (setf *debug* nil)
;; (setf (x *player*) 100 (y *player*) 100)
;; (setf (kit.sdl2:render-enabled *window*) t)
