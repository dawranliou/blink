(in-package #:blink)

(defvar *debug* nil)

(defclass entity ()
  ((entity-id :reader entity-id :initform (gensym "ENTITY-ID-"))))

(defmethod print-object ((entity entity) stream)
  (print-unreadable-object (entity stream :type t)
    (format stream "{~A}" (entity-id entity))))

;;; Data mixins

(defclass pos ()
  ((x :accessor x :initform 0 :initarg :x)
   (y :accessor y :initform 0 :initarg :y)))

(defclass size ()
  ((w :accessor w :initform 0 :initarg :w)
   (h :accessor h :initform 0 :initarg :h)))

(defclass color ()
  ((color :accessor color :initform nil :initarg :color)))

(defclass texture ()
  ((tex :accessor tex :initarg :tex)
   (rect :accessor rect :initarg :rect)))

;;; System

(defvar *renderer*)

(defclass renderable () ())

(defgeneric render (this))

(defun run-render-system (renderer entities camera)
  (let ((*renderer* renderer)
        (*camera* camera))
    (loop :for entity :in entities
          :when (typep entity 'renderable)
            :do (render entity))))

;;; Game Entities

(defclass box (entity renderable pos size color) ())

(defun make-box-entity (x y w h color)
  (make-instance 'box :x x :y y :w w :h h :color color))

(defmethod render ((box box))
  (let ((renderer *renderer*)
        (camera *camera*))
    (with-slots (x y w h color) box
      (apply #'sdl2:set-render-draw-color renderer color)
      (sdl2:render-draw-rect renderer (sdl2:make-rect (- x (x camera))
                                                      (- y (y camera))
                                                      w h)))))

(defclass sprite (entity renderable pos size color texture) ())

(defun make-sprite (tex rect x y w h &key color)
  (make-instance 'sprite :tex tex :rect rect :x x :y y :w w :h h :color color))

(defmethod render ((sprite sprite))
  (let ((renderer *renderer*)
        (camera *camera*))
    (with-slots (tex rect x y w h color) sprite
      (when *debug*
        (apply #'sdl2:set-render-draw-color renderer color)
        (sdl2:render-draw-rect renderer (sdl2:make-rect x y w h)))
      (when color
        (destructuring-bind (r g b _a) color
          (declare (ignore _a))
          (sdl2:set-texture-color-mod (texture tex) r g b)))
      (sdl2:render-copy renderer
                        (texture tex)
                        :source-rect rect
                        :dest-rect (sdl2:make-rect (- x (x camera))
                                                   (- y (y camera))
                                                   w h)))))
