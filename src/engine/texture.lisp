(in-package #:blink)

(defclass tex ()
  ((w :accessor w :initform 0)
   (h :accessor h :initform 0)
   (texture :accessor texture :initform nil)))

(defun load-texture-from-file (pathname)
  (let ((tex (make-instance 'tex)))
    (with-slots (texture w h) tex
      (let ((surface (sdl2-image:load-image pathname)))
        (setf w (sdl2:surface-width surface))
        (setf h (sdl2:surface-height surface))
        (sdl2:set-color-key surface
                            :true
                            (sdl2:map-rgb (sdl2:surface-format surface)
                                          0 #xFF #xFF))
        (setf texture (create-texture-from-surface surface))
        (sdl2:free-surface surface)))
    tex))
