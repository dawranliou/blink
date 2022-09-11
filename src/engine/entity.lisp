(in-package #:blink)

(defvar *debug* nil)

;; (setf *debug* t)
;; (setf *debug* nil)

(defclass entity ()
  ((entity-id :reader entity-id :initform (gensym "ENTITY-ID-"))))

(defmethod print-object ((entity entity) stream)
  (print-unreadable-object (entity stream :type t)
    (format stream "{~A}" (entity-id entity))))

;;; Data mixins

(defclass rect ()
  ((x :accessor x :initform 0 :initarg :x)
   (y :accessor y :initform 0 :initarg :y)
   (w :accessor w :initform 0 :initarg :w)
   (h :accessor h :initform 0 :initarg :h)))

(defmethod print-object ((rect rect) stream)
  (print-unreadable-object (rect stream :type t)
    (format stream "{~A,~A}" (x rect) (y rect))))

(defun make-rect (x y &key (w +sprite-size+) (h +sprite-size+))
  (make-instance 'rect :x x :y y :w w :h h))

(defclass velocity ()
  ((vx :accessor vx :initform 0 :initarg :vx)
   (vy :accessor vy :initform 0 :initarg :vy)))

(defclass color ()
  ((color :accessor color :initform nil :initarg :color)))

(defclass texture ()
  ((tex :accessor tex :initarg :tex)
   (rect :accessor rect :initarg :rect)
   (flip :accessor flip :initarg :flip :initform nil)))

;;; Game Entities
(defclass text-bubble (entity rect color)
  ((text-string :initarg :text-string :accessor text-string)
   (text-size :initarg :text-size :initform 32 :accessor text-size)))

(defun make-text-bubble (x y w h text-string &optional (text-size 32))
  (make-instance 'text-bubble :x x :y y :w w :h h
                              :text-string text-string
                              :text-size text-size))

(defmethod render (renderer (text-bubble text-bubble) &key camera)
  (with-slots (x y w h color text-string text-size) text-bubble
    (let ((dest-rect (sdl2:make-rect (- x (x camera))
                                     (- y (y camera))
                                     w h)))
      (apply #'sdl2:set-render-draw-color renderer (or color +white+))
      (sdl2:render-draw-rect renderer dest-rect)
      (text renderer text-string (- x (x camera)) (- y (y camera))
            :font (make-font :size text-size)))))

(defclass box (entity rect color) ())

(defun make-box-entity (x y w h color)
  (make-instance 'box :x x :y y :w w :h h :color color))

(defmethod render (renderer (box box) &key camera)
  (with-slots (x y w h color) box
    (apply #'sdl2:set-render-draw-color renderer color)
    (let ((dest-rect (if camera
                         (sdl2:make-rect (- x (x camera)) (- y (y camera)) w h)
                         (sdl2:make-rect x y w h))))
      (sdl2:render-draw-rect renderer dest-rect))))

(defclass sprite (entity rect color texture) ())

(defun make-sprite (tex rect x y w h &key color)
  (make-instance 'sprite :tex tex :rect rect :x x :y y :w w :h h :color color))

(defmethod render (renderer (sprite sprite) &key camera)
  (with-slots (tex rect flip x y w h color) sprite
    (let ((dest-rect (if camera
                         (sdl2:make-rect (- x (x camera)) (- y (y camera)) w h)
                         (sdl2:make-rect x y w h))))
      (when *debug*
        (apply #'sdl2:set-render-draw-color renderer (or color +gray-50+))
        (sdl2:render-draw-rect renderer dest-rect))
      (when color
        (destructuring-bind (r g b _a) color
          (declare (ignore _a))
          (sdl2:set-texture-color-mod (texture tex) r g b)))
      (sdl2:render-copy-ex renderer
                           (texture tex)
                           :source-rect rect
                           :dest-rect dest-rect
                           :flip flip))))
