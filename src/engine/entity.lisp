(in-package #:blink)

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

(defun make-rect (x y w h)
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
