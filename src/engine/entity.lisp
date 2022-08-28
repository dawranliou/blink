(in-package #:blink)

(defvar *debug* nil)
(defvar *entity-id-counter* 0)
(defvar *entity-index* (make-hash-table))

(defclass entity ()
  ((id :accessor entity-id :initform (incf *entity-id-counter*))
   (components :accessor entity-components :initarg :components)))

(defun make-entity (entity-sym components)
  (let ((entity (make-instance entity-sym :components components)))
    (setf (gethash (entity-id entity) *entity-index*) entity)
    entity))

(defun destroy-entities ()
  (loop :for entity :being :the :hash-values :of *entity-index*
        :do (remhash (entity-id entity) *entity-index*)))

;;(destroy-entities)

(defclass component () ())

(defgeneric render-component (renderer this &key &allow-other-keys))

(defun run-render-system (renderer camera)
  (loop :for entity :being :the :hash-values :of *entity-index*
        :do (loop :for component :in (entity-components entity)
                  :do (render-component renderer component :camera camera))))

;; concrete components

(defclass box (component)
  ((x :accessor x :initarg :x)
   (y :accessor y :initarg :y)
   (w :accessor w :initarg :w)
   (h :accessor h :initarg :h)
   (color :accessor color :initarg :color :initform +white+)))

(defmethod render-component (renderer (box box) &key camera)
  (with-slots (x y w h color) box
    (apply #'sdl2:set-render-draw-color renderer color)
    (sdl2:render-draw-rect renderer (sdl2:make-rect (- x (x camera))
                                                    (- y (y camera))
                                                    w h))))

(defun make-box-component (x y w h &optional (color +white+))
  (make-instance 'box :x x :y y :w w :h h :color color))

(defclass sprite (component)
  ((tex :accessor tex :initarg :tex)
   (rect :accessor rect :initarg :rect)
   (x :accessor x :initarg :x)
   (y :accessor y :initarg :y)
   (w :accessor w :initarg :w)
   (h :accessor h :initarg :h)
   (tint :accessor tint :initarg :tint :initform nil)))

(defmethod render-component (renderer (sprite sprite) &key camera)
  (with-slots (tex rect x y w h tint) sprite
    (when *debug*
      (with-slots (x y w h tint) sprite
        (apply #'sdl2:set-render-draw-color renderer tint)
        (sdl2:render-draw-rect renderer (sdl2:make-rect x y w h))))
    (when tint
      (destructuring-bind (r g b _a) tint
        (declare (ignore _a))
        (sdl2:set-texture-color-mod (texture tex) r g b)))
    (sdl2:render-copy renderer
                      (texture tex)
                      :source-rect rect
                      :dest-rect (sdl2:make-rect (- x (x camera))
                                                 (- y (y camera))
                                                 w h))))

(defun make-sprite-component (tex rect x y w h &key tint)
  (make-instance 'sprite :tex tex :rect rect :x x :y y :w w :h h :tint tint))
