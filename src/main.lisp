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

(defclass title-scene (scene)
  ()
  (:default-initargs
   :w +width+
   :h +height+))

(defun make-title-scene ()
  (make-instance 'title-scene))

(defmethod init ((title-scene title-scene) &key renderer &allow-other-keys)
  (setf (pause-menu-items title-scene)
        (list (make-text-texture renderer
                                 "Resume"
                                 :resource-pool (resources title-scene))
              (make-text-texture renderer
                                 "Exit"
                                 :resource-pool (resources title-scene)))))

(defmethod update ((title-scene title-scene) &key keys &allow-other-keys)
  (when (gethash "X" keys)
    (transition-to-scene *window* (make-level 'A))))

(defmethod render (renderer (title-scene title-scene) &key)
  (let ((title-font (make-font (resources title-scene) :size 64))
        (small-font (make-font (resources title-scene) :size 24)))
    (text renderer "Spirited" 100 200 :font title-font)
    (text renderer "Act. 2" 100 260 :font title-font)
    (when (zerop (mod (floor (frames title-scene) 20) 2))
      (text renderer "Press <X> to start" 100 350 :font small-font))))

;;; End Scene
(defclass end-scene (scene)
  ()
  (:default-initargs
   :w +width+
   :h +height+))

(defmethod update ((end-scene end-scene) &key keys &allow-other-keys)
  (when (gethash "X" keys)
    (transition-to-scene *window* (make-title-scene))))

(defmethod render (renderer (end-scene end-scene) &key)
  (text renderer "The End" 100 200 :resource-pool (resources end-scene))
  (when (zerop (mod (floor (frames end-scene) 20) 2))
    (text renderer "Press <X> to restart" 100 350
          :resource-pool (resources end-scene))))

;;; Level Scene

(defclass level-scene (scene)
  ((room-sym :initarg :room-sym :accessor room-sym)
   (tiles :initarg :tiles :accessor tiles)
   (portals :initarg :portals :accessor portals)
   ;; Somewhere around the top-left corner
   (player-init-animation :initarg :player-init-animation
                          :accessor player-init-animation
                          :initform :idle)
   (player-init-flip :initarg :player-init-flip
                     :accessor player-init-flip
                     :initform nil)
   (player-init-x :initarg :player-init-x
                  :accessor player-init-x
                  :initform (* 4 +sprite-size+))
   (player-init-y :initarg :player-init-y
                  :accessor player-init-y
                  :initform (* 4 +sprite-size+)))
  (:default-initargs
   :w +width+
   :h +height+))

(defmethod print-object ((level-scene level-scene) stream)
  (print-unreadable-object (level-scene stream :type t)
    (format stream "{ROOM:~A}" (room-sym level-scene))))

(defun room->tiles (room-sym)
  (case room-sym
    (A +room-a+)
    (B +room-b+)
    (C +room-c+)))

(defun make-level (room-sym &key (player-x (* 4 +sprite-size+))
                              (player-y (* 4 +sprite-size+))
                              (player-flip nil)
                              (player-animation :idle))
  (make-instance 'level-scene
                 :room-sym room-sym
                 :tiles (room->tiles room-sym)
                 :player-init-animation player-animation
                 :player-init-flip player-flip
                 :player-init-x player-x
                 :player-init-y player-y))


(defun transition-room (from-room to-room)
  (case (intern (format nil "~A->~A" from-room to-room) "KEYWORD")
    (:A->B (list (* 1 +sprite-size+) (* 14 +sprite-size+) nil))
    (:A->C (list (* 1 +sprite-size+) (* 4 +sprite-size+) nil))
    (:B->A (list (* 22 +sprite-size+) (* 6 +sprite-size+) '(:horizontal)))
    (:C->A (list (* 22 +sprite-size+) (* 24 +sprite-size+) '(:horizontal)))))

(defmethod init ((level-scene level-scene) &key renderer)
  (setf (camera level-scene)
        (make-bounded-camera (* (length (first (tiles level-scene)))
                                +sprite-size+)
                             (* (length (tiles level-scene)) +sprite-size+)
                             +width+
                             +height+))
  (center-scene-camera level-scene
                       :x (player-init-x level-scene)
                       :y (player-init-y level-scene))

  (setf (pause-menu-items level-scene)
        (list (make-text-texture renderer
                                 "Resume"
                                 :resource-pool (resources level-scene))
              (make-text-texture renderer
                                 "Exit"
                                 :resource-pool (resources level-scene))))

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
                              (player-init-x level-scene)
                              (player-init-y level-scene)))
  (set-animation *player* (player-init-animation level-scene))
  (setf (flip *player*) (player-init-flip level-scene))
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
    ;; Portal player to room
    (let ((portal-sym (first (collide-with-portal tiles *player*))))
      (when portal-sym
        (case portal-sym
          ;; (format t "PORTAL ~A~%" portal-sym)
          (Z (transition-to-scene *window* (make-instance 'end-scene)))
          (otherwise
           (let ((room-data (transition-room (room-sym level-scene)
                                             portal-sym)))
             (when room-data
               (destructuring-bind (init-x init-y init-flip) room-data
                 (transition-to-scene *window*
                                      (make-level portal-sym
                                                  :player-animation :run
                                                  :player-x init-x
                                                  :player-y init-y
                                                  :player-flip init-flip)))))))))

    ;; update player state
    (with-slots (x y w h groundedp) *player*
      (setf groundedp
            (collide-with-tile-at-rect tiles (make-rect x (+ y 1) :w w :h h))))

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
        (let ((tiles (collide-with-tile-at-rect tiles *player*)))
          (when tiles
            (loop :for tile :in tiles
                  :do (if (< 0 vx)
                          (setf (x *player*) (- (x tile) (w *player*)))
                          (setf (x *player*) (+ (x tile) (w tile)))))
            (setf (vx *player*) 0)))
        ;; Y collision
        (setf (y *player*) target-y)
        (let ((tiles (collide-with-tile-at-rect tiles *player*)))
          (when tiles
            (loop :for tile :in tiles
                  :do (if (< 0 vy)
                          (setf (y *player*) (- (y tile) (h *player*)))
                          (setf (y *player*) (+ (y tile) (h tile)))))
            (setf (vy *player*) 0)))))))

(defmethod update :after ((level-scene level-scene) &key &allow-other-keys)
  (with-slots (x y) *player*
    (center-scene-camera level-scene :x x :y y)))

(defun run (&key (w +width+) (h +height+))
  (kit.sdl2:init)
  (sdl2:in-main-thread nil
    (setf *window*
          (make-instance 'game-window
                         :title "Blink!"
                         :w w :h h
                         :init-scene (make-title-scene))))
  (kit.sdl2:start)
  *window*)

(defun main ()
  (sdl2:make-this-thread-main
   (lambda () (run))))

#|
(add-tiles-to-scene (scene *window*) +room-a+)
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
(transition-to-scene *window* (make-title-scene))
(transition-to-scene *window* (make-level 'A))
(transition-to-scene *window* (make-level 'C))
(transition-to-scene *window*
                     (make-level 'B
                                 :player-x +sprite-size+
                                 :player-y (* 14 +sprite-size+)))
(run)
(setf (kit.sdl2:render-enabled *window*) t)
|#
