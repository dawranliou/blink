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
    (1 2 0 0 0 0 0 0 0 1 2 1 2 0 0 0 0 0 1 2 0 0 0 0 0)
    (2 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 1 0 0 0 0 0)
    (1 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 2 0 0 0 0 0)
    (2 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 1 0 0 0 0 0)
    (1 2 0 0 1 2 1 2 0 0 0 0 0 0 0 0 0 0 1 2 0 0 0 0 0)
    (2 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 1 0 0 0 0 0)
    (1 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 2 0 0 0 0 0)
    (2 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 1 0 0 0 0 0)
    (1 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 2 0 0 0 0 0)
    (2 1 0 0 0 0 0 0 0 0 0 0 2 1 2 1 0 0 2 1 0 0 0 0 0)
    (1 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 2 1 2 1 2 1)
    (2 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 1 2 1 2 1 2)
    (1 2 0 0 1 2 1 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 1)
    (2 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 2)
    (1 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 1)
    (2 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 2)
    (1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1)
    (2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2)))

(defvar *player-tex* nil)
(defvar *player* nil)
(defvar *bg-tex* nil)

;;; Title Scene

(defclass title-scene (scene) ())

(defmethod update ((title-scene title-scene) &key keys &allow-other-keys)
  (when (gethash "Z" keys)
    (transition-to-scene *window* (make-instance 'level-scene))))

(defmethod render (renderer (title-scene title-scene) &key)
  (let ((title-font (make-font :size 64))
        (small-font (make-font :size 24)))
    (text renderer "Spirited" 100 200 :font title-font)
    (text renderer "Act. 2" 100 260 :font title-font)
    (when (zerop (mod (floor (frames *window*) 20) 2))
      (text renderer "Press <Z> to start" 100 350 :font small-font))))

;;; Level Scene

(defclass level-scene (scene) ()
  ;; (:default-initargs
  ;;  :camera (make-instance 'lerp-camera))
  )

(defmethod init ((level-scene level-scene) &key renderer)
  (setf (camera level-scene) (make-bounded-camera *tiles*
                                                  +sprite-size+
                                                  +width+
                                                  +height+))
  (setf *bg-tex* (load-texture-from-file
                  renderer
                  (relative-path #P"assets/bg.png")))
  (add-tiles-to-scene level-scene *tiles*)

  (setf *player-tex* (load-texture-from-file
                      renderer
                      (relative-path #P"assets/player.png")))
  (setf *player* (make-player *player-tex*
                              (* 4 +sprite-size+)
                              (* 10 +sprite-size+)))
  (add-to-scene level-scene *player*))

(defmethod unload ((level-scene level-scene) &key &allow-other-keys)
  (setf *player-tex* nil)
  (setf *bg-tex* nil))

(defmethod update ((level-scene level-scene) &key keys &allow-other-keys)
  (with-slots (dt) level-scene
    ;; update player state
    (with-slots (x y w h groundedp) *player*
      (setf groundedp
            (collide-with-tile *tiles* (make-rect x (+ y 1) :w w :h h))))

    ;; update horizontal speed
    (cond
      ((gethash "Right" keys) (progn (setf (vx *player*) +0.5
                                           (flip *player*) nil)
                                     (set-animation *player* :run)))
      ((gethash "Left" keys) (progn (setf (vx *player*) -0.5
                                          (flip *player*) '(:horizontal))
                                    (set-animation *player* :run)))
      (t (setf (vx *player*) 0)
         (set-animation *player* :idle)))

    ;; update vertical speed
    (if (groundedp *player*)
        (cond
          ((gethash "Z" keys) (jump *player*))
          (t (setf (vy *player*) 0)))
        (free-fall *player* dt))

    (tick-animator *player* dt)

    ;; collision detection
    (with-slots (x y vx vy w h) *player*
      (let ((target-x (+ x (floor (* dt vx))))
            (target-y (+ y (floor (* dt vy)))))
        ;; X collision
        (setf (x *player*) target-x)
        (let ((tiles (collide-with-tile *tiles* *player*)))
          (when tiles
            (loop :for tile :in tiles
                  :do (if (< 0 vx)
                          (setf (x *player*) (- (x tile) (w *player*)))
                          (setf (x *player*) (+ (x tile) (w tile)))))
            (setf (vx *player*) 0)))
        ;; Y collision
        (setf (y *player*) target-y)
        (let ((tiles (collide-with-tile *tiles* *player*)))
          (when tiles
            (loop :for tile :in tiles
                  :do (if (< 0 vy)
                          (setf (y *player*) (- (y tile) (h *player*)))
                          (setf (y *player*) (+ (y tile) (h tile)))))
            (setf (vy *player*) 0)))))))

(defmethod update :after ((level-scene level-scene) &key &allow-other-keys)
  (with-slots (x y) *player*
    (set-scene-camera level-scene
                      :x (- x (/ +width+ 2))
                      :y (- y (/ +height+ 2)))))

(defun run (&key (w +width+) (h +height+))
  (kit.sdl2:init)
  (sdl2:in-main-thread nil
    (setf *window*
          (make-instance 'game-window
                         :title "Blink!"
                         :w w :h h
                         :init-scene (make-instance 'title-scene))))
  (kit.sdl2:start)
  *window*)

#|
(run)
(add-tiles-to-scene (scene *window*) *tiles*)
(prog1 (setf *player* (make-player *player-tex*
                                   (* 4 +sprite-size+)
                                   (* 10 +sprite-size+)))
  (add-to-scene (scene *window*) *player*))
(remove-all-entities-from-scene (scene *window*))
(setf *debug* t)
(setf *debug* nil)
(setf (x *player*) 100 (y *player*) 100)
(setf (x *player*) 511)
(incf (x *player*) +sprite-size+)
(transition-to-scene *window* (make-instance 'title-scene))
(setf (kit.sdl2:render-enabled *window*) t)
|#
