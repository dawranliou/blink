(in-package #:blink)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GLOBAL STATES

(defvar *window* nil)

(defvar *player-tex* nil)
(defvar *player* nil)
(defvar *bg-tex* nil)
(defvar *npc-tex* nil)
(defvar *objects-tex* nil)
(defvar *title-font* nil)
(defvar *subtitle-font* nil)

;;; Title Scene

(defclass title-scene (scene)
  ()
  (:default-initargs
   :w +width+
   :h +height+
   :pause-menu-items '("Resume" "Exit")))

(defun make-title-scene ()
  (make-instance 'title-scene))

(defmethod init ((title-scene title-scene) &key &allow-other-keys)
  (setf *title-font* (make-font :size 64))
  (setf *subtitle-font* (make-font :size 24)))

(defmethod update ((title-scene title-scene) &key keys &allow-other-keys)
  (when (gethash "X" keys)
    (transition-to-scene *window* (make-level 'A))))

(defmethod render ((title-scene title-scene) &key)
  (text "Spirited" 100 200 :font *title-font*)
  (text "Act. 2" 100 260 :font *subtitle-font*)
  (when (zerop (mod (floor (frames title-scene) 20) 2))
    (text "Press <X> to start" 100 350)))

;;; End Scene
(defclass end-scene (scene)
  ()
  (:default-initargs
   :w +width+
   :h +height+
   :pause-menu-items '("Resume" "Exit")))

(defmethod update ((end-scene end-scene) &key keys &allow-other-keys)
  (when (gethash "X" keys)
    (transition-to-scene *window* (make-title-scene))))

(defmethod render ((end-scene end-scene) &key)
  (text "The End" 100 200)
  (when (zerop (mod (floor (frames end-scene) 20) 2))
    (text "Press <X> to restart" 100 350)))

;;; Level Scene

(defclass level-scene (scene)
  ((room-sym :initarg :room-sym :accessor room-sym)
   (tiles :initarg :tiles :accessor tiles)
   (portals :initarg :portals :accessor portals)
   ;; Conversations
   (in-conversation-p :accessor in-conversation-p :initform nil)
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
   :h +height+
   :pause-menu-items '("Resume" "Exit")))

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
    (:A->B (list (* 1 +sprite-size+) (+ 12 (* 14 +sprite-size+)) nil))
    (:A->C (list (* 1 +sprite-size+) (+ 12 (* 4 +sprite-size+)) nil))
    (:B->A (list (* 22 +sprite-size+) (+ 12 (* 6 +sprite-size+)) '(:horizontal)))
    (:C->A (list (* 22 +sprite-size+) (+ 12 (* 24 +sprite-size+)) '(:horizontal)))))

(defmethod init ((level-scene level-scene) &key)
  (setf (camera level-scene)
        (make-bounded-camera (* (length (first (tiles level-scene)))
                                +sprite-size+)
                             (* (length (tiles level-scene)) +sprite-size+)
                             +width+
                             +height+))
  (center-scene-camera level-scene
                       :x (player-init-x level-scene)
                       :y (player-init-y level-scene))

  (setf *objects-tex* (load-resource
                       (relative-path #P"assets/objects.png")
                       :type :image))

  (setf *player-tex* (load-resource
                      (relative-path #P"assets/player.png")
                      :type :image))
  (setf *player* (make-player *player-tex*
                              (player-init-x level-scene)
                              (player-init-y level-scene)))
  (set-animation *player* (player-init-animation level-scene))
  (setf (flip *player*) (player-init-flip level-scene))
  (add-to-scene level-scene *player*)

  (setf *npc-tex* (load-resource
                   (relative-path #P"assets/npcs.png")
                   :type :image))
  (add-to-scene level-scene
                (make-npc *npc-tex* 0 640 768
                          :active-sprite
                          (make-sprite *objects-tex*
                                       (sdl2:make-rect 16 0 16 16)
                                       (+ 640 16)
                                       (- 768 32)
                                       32 32)
                          :interact-sprite
                          (make-sprite *objects-tex*
                                       (sdl2:make-rect 0 0 16 16)
                                       (+ 640 16)
                                       (- 768 32)
                                       32 32)
                          :conversations
                          '("Howdy!")))

  (setf *bg-tex* (load-resource
                  (relative-path #P"assets/bg.png")
                  :type :image))
  (add-tiles-to-scene level-scene (tiles level-scene) *bg-tex*))

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
            (collide-with-tile-at-rect tiles (make-rect x (+ y 1) w h))))

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

    (run-interactor-system (entities level-scene)
                           *player*
                           :interactp (gethash "X" keys))

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
                         :resizable t
                         :init-scene (make-title-scene))))
  (kit.sdl2:start)
  *window*)

(defun main ()
  (sdl2:make-this-thread-main
   (lambda () (run))))
