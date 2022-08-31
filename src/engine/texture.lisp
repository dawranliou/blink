(in-package #:blink)

(defclass tex ()
  ((renderer :initarg :renderer :initform (error "Must supply a renderer"))
   (w :accessor w :initform 0)
   (h :accessor h :initform 0)
   (texture :accessor texture :initform nil)))

(defun load-texture-from-file (renderer pathname)
  (let ((tex (make-instance 'tex :renderer renderer)))
    (with-slots (renderer texture w h) tex
      (let ((surface (sdl2-image:load-image pathname)))
        (setf w (sdl2:surface-width surface))
        (setf h (sdl2:surface-height surface))
        (sdl2:set-color-key surface
                            :true
                            (sdl2:map-rgb (sdl2:surface-format surface)
                                          0 #xFF #xFF))
        (setf texture (sdl2:create-texture-from-surface renderer surface))
        (sdl2:free-surface surface)))
    tex))
