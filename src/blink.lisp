;;;; blink.lisp

(in-package #:blink)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CONSTANTS

(defparameter +width+ 600)
(defparameter +height+ 600)
(defparameter +sweat-pos+ '((450 150) (100 200) (300 100) (400 400)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GLOBAL STATES

(defvar *scene*)
(defvar *modal*)
(defvar *level*)
(defvar *assets-path* (asdf:system-relative-pathname :blink #p"assets/"))
(defvar *head-texture*)
(defvar *enemies-texture*)
(defvar *sweat-texture*)
(defvar *squint-animation*)
(defvar *idle-animation*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CONDITIONS

(define-condition game-over (error)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GENERICS

(defgeneric update (thing dt)
  (:documentation "Progress the object by `dt' second.")
  (:method (thing dt)))

(defgeneric draw (thing)
  (:documentation "Render the thing on the screen.")
  (:method (thing)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ANIMATION

;; loop?
;; spec: frame cond, texture, src rec, target rec

(defclass animation ()
  ((timer :initform 0 :accessor timer)
   (loop-at :initform nil :initarg :loop-at :accessor loop-at)
   (texture :initarg :texture :accessor texture)
   (target :initarg :target :accessor target)
   (spec :initarg :spec :accessor spec)))

(defmethod update ((this animation) dt)
  (incf (timer this) dt)
  (when (and (loop-at this)
             (< (loop-at this) (timer this)))
    (setf (timer this) 0)))

(defmethod draw ((this animation))
  (loop :named spec-loop
        :for (timer-min timer-max src-rec) :in (spec this)
        :when (and (<= timer-min (timer this))
                   (or (null timer-max)
                       (< (timer this) timer-max)))
          :do (r:draw-texture-pro (texture this) ; *head-texture*
                                  src-rec
                                  (target this)
                                  (r:make-vector2 :x 0.0 :y 0.0)
                                  0.0
                                  r:+white+)))

(defun reset (animation)
  (setf (timer animation) 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ENTITIES

(defclass bug ()
  ((x :initarg :x :accessor x)
   (y :initarg :y :accessor y)
   (v :initarg :v :accessor v)
   (u :initarg :u :accessor u)
   (sprite-id :initform (r:get-random-value 0 11) :accessor sprite-id)
   (alive-p :initform t :accessor alive-p)
   (animation :initarg :animation :accessor animation)))

(defmethod initialize-instance :after ((this bug) &key)
  (setf (animation this)
        (make-instance 'animation
                       :texture *enemies-texture*
                       :spec `((0 0.5
                                ,(r:make-rectangle :x 0
                                                     :y (* 16 (sprite-id this))
                                                     :width 16 :height 16))
                               (0.5 1
                                ,(r:make-rectangle :x 16
                                                   :y (* 16 (sprite-id this))
                                                   :width 16 :height 16)))
                       :target (r:make-rectangle :x (x this)
                                                 :y (y this)
                                                 :width 100
                                                 :height 100)
                       :loop-at 1)))
                       

(defun spawn-bug ()
  (let ((theta (float (r:get-random-value 0 360) pi)))
    (make-instance 'bug
                   :x 300
                   :y 500
                   :v (* (cos theta) 500)
                   :u (* (sin theta) 500))))

(defun die (bug)
  (setf (alive-p bug) nil
        (v bug) 0
        (u bug) 0

        (animation bug)
        (make-instance 'animation
                       :texture *sweat-texture*
                       :spec `((0 0.6
                                  ,(r:make-rectangle :x 0 :y 0
                                                     :width 16 :height 32))
                               (0.6 0.8
                                    ,(r:make-rectangle :x 16 :y 0
                                                       :width 16 :height 32))
                               (0.8 1.0
                                    ,(r:make-rectangle :x 32 :y 0
                                                       :width 16 :height 32)))
                       :target (r:make-rectangle :x (x bug)
                                                 :y (y bug)
                                                 :width 100
                                                 :height 200)
                       :loop-at 1.0)))

(defmethod update ((obj bug) dt)
  (with-slots (x y v u) obj
    (setf x (+ x (* v dt)) y (+ y (* u dt)))

    (cond
      ((< x 0) (setf x 0 v (* -1 v)))
      ((< +width+ x) (setf x +width+ v (* -1 v)))
      ((< y 0) (setf y 0 u (* -1 u)))
      ((< +height+ y) (setf y +height+ u (* -1 u)))))

  (update (animation obj) dt)
  (setf (r:rectangle-x (target (animation obj))) (- (round (x obj)) 50)
        (r:rectangle-y (target (animation obj))) (- (round (y obj)) 50)))

(defmethod draw ((obj bug))
  ;; (with-slots (x y) obj
  ;;   (r:draw-circle (round x) (round y) 5.0 r:+red+))
  (draw (animation obj)))

(defclass sweat ()
  ((x :initarg :x :accessor x)
   (y :initarg :y :accessor y)
   (animation :initarg :animation :accessor animation)))

(defmethod initialize-instance :after ((this sweat) &key)
  (setf (animation this)
        (make-instance 'animation
                       :texture *sweat-texture*
                       :spec `((0 0.5
                                ,(r:make-rectangle :x 0 :y 0
                                                   :width 16 :height 32))
                               (0.5 1
                                ,(r:make-rectangle :x 16 :y 0
                                                   :width 16 :height 32)))
                       :target (r:make-rectangle :x (x this)
                                                 :y (y this)
                                                 :width 100
                                                 :height 200)
                       :loop-at 1)))

(defmethod update ((obj sweat) dt)
  (update (animation obj) dt)
  (setf (r:rectangle-x (target (animation obj))) (- (round (x obj)) 50)
        (r:rectangle-y (target (animation obj))) (- (round (y obj)) 50)))

(defmethod draw ((obj sweat))
  (draw (animation obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SCENES

(defclass scene ()
  ((frame :initform 0 :accessor frame)
   (bg :initarg :bg :initform r:+black+ :accessor bg)))

(defmethod update :before ((scene scene) dt)
  (when (r:is-key-pressed r:+key-escape+)
    (show-exit-modal))
  (incf (frame scene)))

(defmethod draw :before ((scene scene))
  (r:clear-background (bg scene)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MODAL

(defclass modal ()
  ((x :initarg :x :initform 100 :accessor x)
   (y :initarg :y :initform 100 :accessor y)
   (w :initarg :w :initform 400 :accessor w)
   (h :initarg :h :initform 400 :accessor h)
   (bg :initarg :bg :initform r:+black+ :accessor bg)))

(defmethod update :before ((this modal) dt)
  (when (r:is-key-pressed r:+key-escape+)
    (setf *modal* nil)))

(defmethod draw :before ((this modal))
  (r:draw-rectangle 0 0 +width+ +height+ '(0 0 0 155))
  (with-slots (x y w h bg) this
    (r:draw-rectangle x y w h bg)
    (r:draw-rectangle-lines x y w h r:+raywhite+)))

(defun close-modal ()
  (setf *modal* nil))

(defclass dialog-modal (modal)
  ((text :initarg :text :accessor text)))

(defmethod update ((this dialog-modal) dt)
  (when (or (r:is-key-pressed r:+key-enter+)
            (r:is-key-pressed r:+key-x+)
            (r:is-key-pressed r:+key-z+))
    (close-modal)))

(defmethod draw ((this dialog-modal))
  (r:draw-text (text this) 150 200 30 r:+raywhite+))

(defun show-dialog-modal (text)
  (setf *modal* (make-instance 'dialog-modal :text text)))

(defclass y-n-modal (modal)
  ((text :initarg :text :accessor text)
   (yes :initarg :yes :initform nil :accessor yes)
   (y-action :initarg :y-action :accessor y-action)
   (n-action :initarg :n-action :accessor n-action)))

(defmethod update ((this y-n-modal) dt)
  (when (r:is-key-pressed r:+key-left+)
    (setf (yes this) t))
  (when (r:is-key-pressed r:+key-right+)
    (setf (yes this) nil))
  (when (or (r:is-key-pressed r:+key-enter+)
            (r:is-key-pressed r:+key-x+)
            (r:is-key-pressed r:+key-z+))
    (if (yes this)
        (funcall (y-action this))
        (funcall (n-action this)))))

(defmethod draw ((this y-n-modal))
  (r:draw-text (text this) 150 200 30 r:+raywhite+)
  (r:draw-text "Yes" 180 240 30 r:+raywhite+)
  (r:draw-text "No" 330 240 30 r:+raywhite+)
  (if (yes this)
      (r:draw-text ">" 150 240 30 r:+raywhite+)
      (r:draw-text ">" 300 240 30 r:+raywhite+)))

(defun show-exit-modal ()
  (setf *modal* (make-instance 'y-n-modal
                               :text "Do you want to quit?"
                               :yes nil
                               :y-action (lambda () (signal 'game-over))
                               :n-action (lambda () (close-modal)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TITLE SCENE
(defclass title-scene (scene)
  ())

(defmethod update ((scene title-scene) dt)
  (when (< 180 (frame scene))
    (setf *scene* (make-instance 'menu-scene))))

(defmethod draw ((scene title-scene))
  (r:draw-text "Blink" 100 200 100 r:+raywhite+)
  (if (evenp (mod (floor (frame scene) 60) 2))
      (r:draw-text ";" 400 180 100 r:+raywhite+)
      (r:draw-text ":" 400 200 100 r:+raywhite+))
  (r:draw-text " )" 400 200 100 r:+raywhite+))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MENU SCENE

(defclass menu-scene (scene)
  ((options :initform '("Start" "Option" "Quit") :accessor options)
   (selection :initform 0 :accessor selection)))

(defmethod update ((scene menu-scene) dt)
  (with-slots (options selection) scene
    (when (and (r:is-key-pressed r:+key-up+)
               (< 0 selection))
      (decf (selection scene)))
    (when (and (r:is-key-pressed r:+key-down+)
               (< selection (1- (length options))))
      (incf selection))

    (when (or (r:is-key-pressed r:+key-enter+)
              (r:is-key-pressed r:+key-x+)
              (r:is-key-pressed r:+key-z+))
      (ecase selection
        (0 (setf *scene* (make-instance 'level-scene)))
        (1 (show-dialog-modal "Options"))
        (2 (show-exit-modal))))))

(defmethod draw ((scene menu-scene))
  (r:draw-text ">" 50 (+ 100 (* 100 (1+ (selection scene)))) 100 r:+raywhite+)
  (loop :for idx :from 1
        :for opt :in (options scene)
        :do (r:draw-text opt 100 (+ 100 (* idx 100)) 100 r:+raywhite+)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; LEVEL SCENE

(defclass level-scene (scene)
  ((n :initform *level* :accessor n)))

(defmethod update ((this level-scene) dt)
  (when (r:is-key-pressed r:+key-z+)
    (setf *scene* (make-instance 'arena-scene))))

(defmethod draw ((this level-scene))
  (r:draw-text (format nil "Level ~a" (n this))
               100 100 100 r:+raywhite+)
  (when (evenp (mod (floor (frame this) 60) 2))
    (r:draw-text "To start press [Z]" 100 200 30 r:+raywhite+)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GAME OVER SCENE

(defclass game-over-scene (scene)
  ((n :initform *level* :accessor n)))

(defmethod update ((game-over-scene scene) dt)
  (when (or (r:is-key-pressed r:+key-enter+)
            (r:is-key-pressed r:+key-x+)
            (r:is-key-pressed r:+key-z+))
    (setf *scene* (make-instance 'level-scene))))

(defmethod draw ((this game-over-scene))
  (r:draw-text "Game Over" 100 100 80 r:+raywhite+)
  (r:draw-text (format nil "You've made it to level ~a!" (n this))
               100 200 30 r:+raywhite+)
  (when (evenp (mod (floor (frame this) 60) 2))
    (r:draw-text (format nil "Continue? [Z]")
                 100 300 30 r:+raywhite+)))

(defun game-over-sequence ()
  (setf *scene* (make-instance 'game-over-scene)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ARENA SCENE

(defclass arena-scene (scene)
  ((eyelids :initform 0.0 :accessor eyelids)
   (state :initform :idle :accessor state)
   (level :initform *level* :accessor level)
   (bugs :initform (make-hash-table) :accessor bugs)
   (sweats :initform (make-hash-table) :accessor sweats)
   (timer :initform 5 :accessor timer)
   (meter :initform 0 :accessor meter)
   (hp :initform 3 :accessor hp)
   (animation :initform *idle-animation* :accessor animation)))

(defmethod initialize-instance :after ((obj arena-scene) &key)
  (dotimes (n (level obj))
    (setf (gethash n (bugs obj)) (spawn-bug))))

(defmethod update ((this arena-scene) dt)
  (let ((action-p (r:is-key-down r:+key-z+)))
    (case (state this)
      (:idle (if action-p
                 (setf (state this) :squint
                       (animation this) *squint-animation*
                       (timer *squint-animation*) 0)
                 (setf (state this) :idle)))
      (:squint (if action-p
                   (setf (state this) :squint)
                   (setf (state this) :idle
                         (animation this) *idle-animation*
                         (timer *idle-animation*) 0)))))

  (update (animation this) dt)

  (unless (eql :squint (state this))
    (loop :for bug-id :being :the :hash-key
            :using (hash-value bug)
              :of (bugs this)
          :when (alive-p bug)
            :do (when (or
                       (r:check-collision-circles
                        (r:make-vector2 :x (round (x bug)) :y (round (y bug)))
                        5.0
                        (r:make-vector2 :x 200 :y 280)
                        80.0)
                       (r:check-collision-circles
                        (r:make-vector2 :x (round (x bug)) :y (round (y bug)))
                        5.0
                        (r:make-vector2 :x 400 :y 280)
                        80.0))
                  (remhash bug-id (bugs this))
                  (let* ((spawn-idx (hash-table-count (sweats this)))
                         (spawn-pos (nth spawn-idx +sweat-pos+)))
                    (setf (gethash bug-id (sweats this))
                          (make-instance 'sweat
                                         :x (car spawn-pos)
                                         :y (cadr spawn-pos))))
                  (decf (hp this)))))

  (when (< (hp this) 0)
    (game-over-sequence))
  (decf (timer this) dt)

  (with-slots (meter state) this
    (case state
      (:squint (setf (eyelids this) (min 1.0 (+ (eyelids this) (* 5 dt)))
                     meter (min 1.0 (+ meter dt))))
      (:idle (setf (eyelids this) (max 0.0 (- (eyelids this) (* 5 dt)))
                   meter (max 0 (- meter (* 1.0 dt)))))))

  (when (< (timer this) 0)
    (incf *level*)
    (setf *scene* (make-instance 'level-scene))
    (return-from update))

  (loop :for bug :being :the :hash-value :of (bugs this)
        :do (update bug dt))

  (loop :for sweat :being :the :hash-value :of (sweats this)
        :do (update sweat dt)))

(defmethod draw ((this arena-scene))
  (draw (animation this))
  ;; eyeballs
  ;; (r:draw-circle 200 280 80.0 r:+raywhite+)
  ;; (r:draw-circle 400 280 80.0 r:+raywhite+)
  ;; pupils
  ;; (r:draw-circle 200 280 30.0 r:+black+)
  ;; (r:draw-circle 400 280 30.0 r:+black+)
  ;; eyelids
  ;; (r:draw-rectangle 110 190 380 (round (* (eyelids this) 80)) r:+yellow+)
  ;; (r:draw-rectangle 110 (- 380 (round (* (eyelids this) 80))) 380
  ;;                   (round (* (eyelids this) 80)) r:+yellow+)

  ;; Bugs
  (loop :for k :being :the :hash-key :using (hash-value bug) :of (bugs this)
        :do (draw bug))

  ;; Sweats
  (loop :for sweat :being :the :hash-value :of (sweats this)
        :do (draw sweat))

  ;; Overlay
  (r:draw-rectangle 0 0 +width+ (round (* (eyelids this) 200))
                    r:+black+)
  (r:draw-rectangle 0 (- 600 (round (* (eyelids this) 220)))
                    +width+ (round (* (eyelids this) 220))
                    r:+black+)

  ;; (r:draw-rectangle 0 0 +width+ +height+
  ;;                   (list 0 0 0 (round (* 200 (eyelids this)))))

  ;; Heads-up-display
  (r:draw-rectangle 10 10
                    (round (* (timer this) 1/5 (- +width+ 20)))
                    10 r:+red+)
  (r:draw-rectangle 10 580
                    (round (* (meter this) (- +width+ 20)))
                    10 r:+blue+))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Game

(defun tic ()
  ;; Handle input + progress game states
  (update (or *modal* *scene*) (r:get-frame-time))
  ;; Render
  (r:with-drawing
    (draw *scene*)
    ;; Overlay modal on top of scene
    (when *modal*
      (draw *modal*))))

(defun init-game ()
  "Set game state"
  (setf *scene* (make-instance 'menu-scene))
  (setf *modal* nil)
  (setf *level* 1)
  (setf *head-texture*
        (r:load-texture
         (namestring (merge-pathnames "head.png" *assets-path*))))
  (setf *enemies-texture*
        (r:load-texture
         (namestring (merge-pathnames "enemies.png" *assets-path*))))
  (setf *sweat-texture*
        (r:load-texture
         (namestring (merge-pathnames "sweat.png" *assets-path*))))
  (setf *idle-animation*
        (make-instance 'animation
                       :texture *head-texture*
                       :spec `((0 0.5
                                ,(r:make-rectangle :x 0 :y 0
                                                  :width 64 :height 64))
                               (0.5 1
                                ,(r:make-rectangle :x 0 :y 0
                                                  :width 64 :height 64)))
                       :target (r:make-rectangle :x 100 :y 100
                                                 :width 400 :height 400)
                       :loop-at 1))
  (setf *squint-animation*
        (make-instance 'animation
                       :texture *head-texture*
                       :spec `((0 0.1
                                ,(r:make-rectangle :x 0 :y 0
                                                  :width 64 :height 64))
                               (0.1 0.2
                                ,(r:make-rectangle :x 64 :y 0
                                                  :width 64 :height 64))
                               (0.2 nil
                                ,(r:make-rectangle :x 128 :y 0
                                                  :width 64 :height 64)))
                       :target (r:make-rectangle :x 100 :y 100
                                                 :width 400 :height 400))))

(defun unload-game ()
  (setf *scene* nil)
  (setf *modal* nil)
  (setf *level* nil)
  (setf *head-texture* nil)
  (setf *enemies-texture* nil))

(defun main ()
  (r:with-window (+width+ +height+ "Blink")
    (r:set-target-fps 60)
    (r:set-exit-key 0)                  ; Re-purpose the ESC key
    (format t "INIT GAME")
    (init-game)
    (handler-case
        (loop until (r:window-should-close) do (tic))
      (game-over (c)
        (declare (ignore c))
        (format t "Game Over!~&")))
    (unload-game)))

;; (main)
