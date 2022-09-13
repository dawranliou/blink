(in-package #:blink)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CONSTANTS

(defparameter +sprite-size+ 32)
(defparameter +width+ (* +sprite-size+ 20))
(defparameter +height+ (* +sprite-size+ 18))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GLOBAL STATES

(defvar *window* nil)

(defvar *player-tex* nil)
(defvar *player* nil)
(defvar *bg-tex* nil)
(defvar *npc-tex* nil)
(defvar *objects-tex* nil)

;;; Title Scene

(defclass title-scene (scene) ())

(defmethod update ((title-scene title-scene) &key keys &allow-other-keys)
  (when (gethash "Z" keys)
    (transition-to-scene *window* (make-level "A" +room-1+))))

(defmethod render (renderer (title-scene title-scene) &key)
  (let ((title-font (make-font :size 64))
        (small-font (make-font :size 24)))
    (text renderer "Spirited" 100 200 :font title-font)
    (text renderer "Act. 2" 100 260 :font title-font)
    (when (zerop (mod (floor (frames *window*) 20) 2))
      (text renderer "Press <Z> to start" 100 350 :font small-font))))

;;; Level Scene

(defclass level-scene (scene)
  ((name :initarg :name :accessor name)
   (tiles :initarg :tiles :accessor tiles)
   ;; Somewhere around the top-left corner
   (player-drop-in-x :initarg :player-drop-in-x
                     :accessor player-drop-in-x
                     :initform (* 4 +sprite-size+))
   (player-drop-in-y :initarg :player-drop-in-y
                     :accessor player-drop-in-y
                     :initform (* 4 +sprite-size+))))

(defmethod print-object ((level-scene level-scene) stream)
  (print-unreadable-object (level-scene stream :type t)
    (format stream "{ROOM ~A}" (name level-scene))))

(defun make-level (name tiles &key (player-x (* 4 +sprite-size+))
                                (player-y (* 4 +sprite-size+)))
  (make-instance 'level-scene
                 :name name
                 :tiles tiles
                 :player-drop-in-x player-x
                 :player-drop-in-y player-y))

(defmethod init ((level-scene level-scene) &key renderer)
  (setf (camera level-scene)
        (make-bounded-camera (* (length (first (tiles level-scene)))
                                +sprite-size+)
                             (* (length (tiles level-scene)) +sprite-size+)
                             +width+
                             +height+))
  (setf *objects-tex* (load-texture-from-file
                       renderer
                       (relative-path #P"assets/objects.png")))

  (setf *bg-tex* (load-texture-from-file
                  renderer
                  (relative-path #P"assets/bg.png")))
  (add-tiles-to-scene level-scene (tiles level-scene))

  (setf *player-tex* (load-texture-from-file
                      renderer
                      (relative-path #P"assets/player.png")))
  (setf *player* (make-player *player-tex*
                              (player-drop-in-x level-scene)
                              (player-drop-in-y level-scene)))
  (add-to-scene level-scene *player*)

  (setf *npc-tex* (load-texture-from-file
                   renderer
                   (relative-path #P"assets/npcs.png")))
  (add-to-scene level-scene (make-npc *npc-tex* 0 640 768
                                      :active-sprite
                                      (make-sprite *objects-tex*
                                                   (sdl2:make-rect 0 0 16 16)
                                                   (+ 640 16)
                                                   (- 768 32)
                                                   32 32))))

(defmethod unload ((level-scene level-scene) &key &allow-other-keys)
  (setf *player-tex* nil)
  (setf *bg-tex* nil))

(defmethod update ((level-scene level-scene) &key keys &allow-other-keys)
  (with-slots (dt tiles) level-scene
    ;; update player state
    (with-slots (x y w h groundedp) *player*
      (setf groundedp
            (collide-with-tile tiles (make-rect x (+ y 1) :w w :h h))))

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

    (run-animator-system (entities level-scene) dt)
    (run-interactor-system (entities level-scene) *player*)

    ;; collision detection
    (with-slots (x y vx vy w h) *player*
      (let ((target-x (+ x (floor (* dt vx))))
            (target-y (+ y (floor (* dt vy)))))
        ;; X collision
        (setf (x *player*) target-x)
        (let ((tiles (collide-with-tile tiles *player*)))
          (when tiles
            (loop :for tile :in tiles
                  :do (if (< 0 vx)
                          (setf (x *player*) (- (x tile) (w *player*)))
                          (setf (x *player*) (+ (x tile) (w tile)))))
            (setf (vx *player*) 0)))
        ;; Y collision
        (setf (y *player*) target-y)
        (let ((tiles (collide-with-tile tiles *player*)))
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

(defun main ()
  (sdl2:make-this-thread-main
   (lambda () (run))))

#|
(run)
(add-tiles-to-scene (scene *window*) +room-1+)
(remove-entity-from-scene *scene* *player*)
(setf *player-tex* (load-texture-from-file
                    (renderer *window*)
                    (relative-path #P"assets/player.png")))
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
