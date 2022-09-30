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

(defvar *default-font* nil)

(defun make-default-font ()
  (setf *default-font*
        (or *default-font*
            (make-font :filename "assets/ShareTechMono-Regular.ttf"
                       :size 24))))
