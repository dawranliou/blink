;;;; blink.lisp

(in-package #:blink)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CONSTANTS

(defparameter +width+ 600)
(defparameter +height+ 600)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GLOBAL STATES

(defvar *scene*)
(defvar *modal*)
(defvar *level*)

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
;;; ENTITIES

(defclass bug ()
  ((x :initarg :x :accessor x)
   (y :initarg :y :accessor y)
   (v :initarg :v :accessor v)
   (u :initarg :u :accessor u)
   (alive-p :initform t :accessor alive-p)))

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
        (u bug) 0))

(defmethod update ((obj bug) dt)
  (with-slots (x y v u) obj
    (setf x (+ x (* v dt)) y (+ y (* u dt)))

    (cond
      ((< x 0) (setf x 0 v (* -1 v)))
      ((< +width+ x) (setf x +width+ v (* -1 v)))
      ((< y 0) (setf y 0 u (* -1 u)))
      ((< +height+ y) (setf y +height+ u (* -1 u))))))

(defmethod draw ((obj bug))
  (with-slots (x y) obj
    (r:draw-circle (round x) (round y) 5.0 r:+red+)))

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
   (level :initform *level* :accessor level)
   (bugs :initform (make-hash-table) :accessor bugs)
   (timer :initform 5 :accessor timer)
   (meter :initform 0 :accessor meter)
   (hp :initform 3 :accessor hp)
   (squint-p :initform nil :accessor squint-p)
   (cooldown-p :initform nil :accessor cooldown-p)))

(defmethod initialize-instance :after ((obj arena-scene) &key)
  (dotimes (n (level obj))
    (setf (gethash n (bugs obj)) (spawn-bug))))

(defmethod update ((this arena-scene) dt)
  (let ((squint-p (r:is-key-down r:+key-z+)))
    (cond
      ;; done cooling down
      ((and (cooldown-p this)
            (zerop (meter this)))
       (setf (cooldown-p this) nil))
      ;; keep cooling down
      ((cooldown-p this) nil)
      ;; need to cool down after 1 second
      ;; ((<= 1 (meter this)) (setf (cooldown-p this) t (squint-p this) nil))
      ;; squinting hard
      (squint-p (setf (squint-p this) t (cooldown-p this) nil))
      ;; relaxing
      (t (setf (squint-p this) nil (cooldown-p this) nil)
         (loop :for bug-id :being :the :hash-key
                 :using (hash-value bug)
                   :of (bugs this)
               :when (alive-p bug)
               :do (when (or (r:check-collision-circles
                              (r:make-vector2 :x (round (x bug)) :y (round (y bug)))
                              5.0
                              (r:make-vector2 :x 200 :y 280)
                              80.0)
                             (r:check-collision-circles
                              (r:make-vector2 :x (round (x bug)) :y (round (y bug)))
                              5.0
                              (r:make-vector2 :x 400 :y 280)
                              80.0))
                     (die bug)
                     (decf (hp this)))))))

  (when (< (hp this) 0)
    (game-over-sequence))
  (decf (timer this) dt)

  (with-slots (meter squint-p cooldown-p) this
    (cond
      (squint-p (setf (eyelids this) (min 1.0 (+ (eyelids this) (* 5 dt)))
                      meter (min 1.0 (+ meter dt))))
      (t (setf (eyelids this) (max 0.0 (- (eyelids this) (* 5 dt)))
               meter (max 0 (- meter (* 1.0 dt)))))))

  (when (< (timer this) 0)
    (incf *level*)
    (setf *scene* (make-instance 'level-scene))
    (return-from update))

  (loop :for bug :being :the :hash-value :of (bugs this)
        :do (update bug dt)))

(defmethod draw ((this arena-scene))
  ;; face
  (r:draw-circle 300 300 220.0 r:+yellow+)
  ;; mouse
  (r:draw-line-bezier-quad (r:make-vector2 :x 200.0 :y 380.0)
                           (r:make-vector2 :x 400.0 :y 380.0)
                           (r:make-vector2 :x 300.0 :y 450.0)
                           10.0 r:+black+)
  ;; eye bags
  (when (< (hp this) 3)
    (r:draw-circle 200 280 90.0 r:+black+)
    (r:draw-circle 400 280 90.0 r:+black+))

  ;; eyeballs
  (r:draw-circle 200 280 80.0 r:+raywhite+)
  (r:draw-circle 400 280 80.0 r:+raywhite+)

  (when (< (hp this) 2)
    (r:draw-circle 200 280 80.0 r:+pink+)
    (r:draw-circle 400 280 80.0 r:+pink+))

  (when (< (hp this) 1)
    (r:draw-circle 200 280 50.0 r:+red+)
    (r:draw-circle 400 280 50.0 r:+red+))

  ;; pupils
  (r:draw-circle 200 280 30.0 r:+black+)
  (r:draw-circle 400 280 30.0 r:+black+)
  ;; eyelids
  (r:draw-rectangle 110 190 380 (round (* (eyelids this) 80)) r:+yellow+)
  (r:draw-rectangle 110 (- 380 (round (* (eyelids this) 80))) 380
                    (round (* (eyelids this) 80)) r:+yellow+)
  (when (< (hp this) 3)
    (r:draw-rectangle 120 (+ 190 (round (* (eyelids this) 80)))
                      170 20 r:+black+)
    (r:draw-rectangle 320 (+ 190 (round (* (eyelids this) 80)))
                      170 20 r:+black+))

  ;; Bugs
  (loop :for k :being :the :hash-key :using (hash-value bug) :of (bugs this)
        :do (draw bug))

  ;; Overlay
  (r:draw-rectangle 0 0 +width+ (round (* (eyelids this) 240))
                    r:+black+)
  (r:draw-rectangle 0 (- 600 (round (* (eyelids this) 280)))
                    +width+ (round (* (eyelids this) 280))
                    r:+black+)

  (r:draw-rectangle 0 0 +width+ +height+
                    (list 0 0 0 (round (* 200 (eyelids this)))))

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
  (setf *scene* (make-instance 'title-scene))
  (setf *modal* nil)
  (setf *level* 1))

(defun unload-game ()
  (setf *scene* nil)
  (setf *modal* nil)
  (setf *level* nil))

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
