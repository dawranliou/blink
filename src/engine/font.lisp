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

(defun make-font (resource-pool
                  &key (filename "assets/ShareTechMono-Regular.ttf") (size 18))
  (let ((filename (relative-path filename)))
    (load-resource resource-pool filename :type :font :size size)))

(defun text (renderer text-string x y &key w h font resource-pool)
  (let* ((font-pointer (pointer (or font (make-font resource-pool))))
         (surface (sdl2-ttf:render-text-solid font-pointer
                                              text-string
                                              255 255 255 0))
         (texture (sdl2:create-texture-from-surface renderer surface))
         (destination-rect
           (sdl2:make-rect x y
                           (or w (sdl2:texture-width texture))
                           (or h (sdl2:texture-height texture)))))
    (sdl2:render-copy renderer
                      texture
                      :source-rect (cffi:null-pointer)
                      :dest-rect destination-rect)))
