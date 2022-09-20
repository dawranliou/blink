(ql:quickload '("deploy"
                "cffi-libffi"
                "cl-glut"
                "cl-opengl"))

;; (sb-ext:save-lisp-and-die "blink"
;;                           :executable t
;;                           :compression t
;;                           :toplevel 'blink::main)

#+darwin
(prog1 'darwin
  (deploy:define-library cffi::libffi
    :path "/usr/local/opt/libffi/lib/libffi.dylib")

  (deploy:define-library deploy::libz
    :path "/usr/local/opt/zlib/lib/libz.dylib")

  (deploy:define-library cl-glut::glut
    :path "/opt/X11/lib/libglut.dylib")

  (deploy:define-library cl-opengl-bindings::opengl
    :path "/opt/X11/lib/libgl.dylib")

  (cffi:define-foreign-library libX11
    (T (:default "libX11")))
  (deploy:define-library libX11
    :path "/opt/X11/lib/libX11.dylib")

  (cffi:define-foreign-library libX11-xcb
    (T (:default "libX11-xcb")))
  (deploy:define-library libX11-xcb
    :path "/opt/X11/lib/libX11-xcb.dylib")

  (cffi:define-foreign-library libXext
    (T (:default "libXext")))
  (deploy:define-library libXext
    :path "/opt/X11/lib/libXext.dylib")

  (cffi:define-foreign-library libglapi
    (T (:default "libglapi")))
  (deploy:define-library libglapi
    :path "/opt/X11/lib/libglapi.dylib")

  (cffi:define-foreign-library libxcb
    (T (:default "libxcb")))
  (deploy:define-library libxcb
    :path "/opt/X11/lib/libxcb.dylib")

  (cffi:define-foreign-library libxcb-glx
    (T (:default "libxcb-glx")))
  (deploy:define-library libxcb-glx
    :path "/opt/X11/lib/libxcb-glx.dylib")

  (deploy::define-hook (:build sort-foreign-libraries) ()
    ;; Pretend to sort the list but this is just to ensure LIBSDL2 is loaded
    ;; before LIBSDL2-TTF and LIBSDL2-IMAGE
    (let ((sorted-libraries (reverse deploy::*foreign-libraries-to-reload*)))
      (setf deploy::*foreign-libraries-to-reload* sorted-libraries)
      (deploy::status 1 "Sorted libraries: ~A" sorted-libraries))))

(deploy:define-resource-directory assets "assets/")

(asdf:make :blink)
