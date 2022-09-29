(in-package #:blink)

(defclass tex (resource)
  ((w :accessor w :initform 0)
   (h :accessor h :initform 0)
   (texture :accessor texture :initform nil)))

(defmethod load-typed-resource (filename (type (eql :image)) &key &allow-other-keys)
  (let ((tex (make-instance 'tex)))
    (with-slots (texture w h) tex
      (let ((surface (sdl2-image:load-image filename)))
        (setf w (sdl2:surface-width surface))
        (setf h (sdl2:surface-height surface))
        (sdl2:set-color-key surface
                            :true
                            (sdl2:map-rgb (sdl2:surface-format surface)
                                          0 #xFF #xFF))
        (setf texture (create-texture-from-surface surface))
        (sdl2:free-surface surface)))
    tex))

(defmethod free-resource ((tex tex))
  (sdl2:destroy-texture (texture tex)))
