(ql:quickload '("deploy" "cffi-libffi" "cl-glut" "cl-opengl"))

;; (sb-ext:save-lisp-and-die "blink"
;;                           :executable t
;;                           :compression t
;;                           :toplevel 'blink::main)

#+darwin
(deploy:define-library cffi::libffi
  :path "/usr/local/opt/libffi/lib/libffi.dylib")

#+darwin
(deploy:define-library deploy::libz
  :path "/usr/local/opt/zlib/lib/libz.dylib")

#+darwin
(deploy:define-library cl-glut::glut
  :path "/opt/X11/lib/libglut.dylib")

#+darwin
(deploy:define-library cl-opengl-bindings::opengl
  :path "/opt/X11/lib/libgl.dylib")

#+darwin
(deploy::define-hook (:build sort-foreign-libraries) ()
  ;; Pretend to sort the list but this is just to ensure LIBSDL2 is loaded
  ;; before LIBSDL2-TTF and LIBSDL2-IMAGE
  (let ((sorted-libraries (reverse deploy::*foreign-libraries-to-reload*)))
    (setf deploy::*foreign-libraries-to-reload* sorted-libraries)
    (deploy::status 1 "Sorted libraries: ~A" sorted-libraries)))

(deploy:define-resource-directory assets "assets/")

(asdf:make :blink)
