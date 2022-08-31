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

(let ((font))
  (defun make-default-font ()
    (setf font
          (or font
              (let ((filename (relative-path "assets/PROBE_10PX_OTF.otf")))
                (load-resource filename :type :font :size 18))))))

(defun text (renderer text-string x y &key w h font)
  (let* ((font-pointer (pointer (or font (make-default-font))))
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

