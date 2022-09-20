(ql:quickload '("deploy"
                "cffi-libffi"
                "cl-glut"
                "cl-opengl"))

;; (sb-ext:save-lisp-and-die "blink"
;;                           :executable t
;;                           :compression t
;;                           :toplevel 'blink::main)

#+darwin
(progn
  (deploy:define-library cffi::libffi
    :path "/usr/local/opt/libffi/lib/libffi.dylib")

  (deploy:define-library deploy::libz
    :path "/usr/local/opt/zlib/lib/libz.dylib")

  (deploy:define-library cl-glut::glut
    :path "/opt/X11/lib/libglut.dylib")

  (deploy:define-library cl-opengl-bindings::opengl
    :path "/opt/X11/lib/libgl.dylib")

  (defmacro bundle-mac-x11-dylib (&rest libraries)
    `(progn ,@(loop for lib in libraries
                    collect `(prog1 ,lib
                               (cffi:define-foreign-library ,(read-from-string lib)
                                 (T (:default ,lib)))
                               (deploy:define-library ,(read-from-string lib)
                                 :path ,(format nil "/opt/X11/lib/~A.dylib" lib))))))

  (bundle-mac-x11-dylib "libX11"
                        "libX11-xcb"
                        "libXext"
                        "libglapi"
                        "libxcb"
                        "libXau"
                        "libxcb-glx")

  (deploy::define-hook (:build sort-foreign-libraries) ()
    ;; Pretend to sort the list but this is just to ensure LIBSDL2 is loaded
    ;; before LIBSDL2-TTF and LIBSDL2-IMAGE
    (let ((sorted-libraries (reverse deploy::*foreign-libraries-to-reload*)))
      (setf deploy::*foreign-libraries-to-reload* sorted-libraries)
      (deploy::status 1 "Sorted libraries: ~A" sorted-libraries))))

(deploy:define-resource-directory assets "assets/")

(asdf:make :blink)
