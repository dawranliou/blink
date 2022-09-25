(in-package #:blink)

(defclass font (resource)
  ((filename :accessor filename :initarg :filename)
   (pointer :accessor pointer :initarg :pointer)
   (size :accessor size :initarg :size :initform 16)))

(defmethod load-typed-resource (filename (type (eql :font))
                                &key (size 18) &allow-other-keys)
  (make-instance 'font
                 :size size
                 :filename filename
                 :pointer (sdl2-ttf:open-font filename
                                              (coerce (truncate size)
                                                      '(signed-byte 32)))))

(defmethod free-resource ((font font))
  (sdl2-ttf:close-font (pointer font)))

(defun make-font (&key (filename "assets/ShareTechMono-Regular.ttf") (size 24))
  (let ((filename (relative-path filename)))
    (load-resource filename :type :font :size size)))

(defun text (text-string x y &key w h font)
  (let* ((font-pointer (pointer (or font (make-font))))
         (surface (sdl2-ttf:render-text-blended font-pointer
                                                text-string
                                                255 255 255 0))
         (texture (create-texture-from-surface surface))
         (destination-rect
           (sdl2:make-rect x y
                           (or w (sdl2:texture-width texture))
                           (or h (sdl2:texture-height texture)))))
    (render-copy texture destination-rect)))

(defun make-text-texture (text-string &key font)
  (let* ((font-pointer (pointer (or font (make-font))))
         (surface (sdl2-ttf:render-text-blended font-pointer
                                                text-string
                                                255 255 255 0)))
    (create-texture-from-surface surface)))
