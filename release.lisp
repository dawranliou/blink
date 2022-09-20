(ql:quickload '("deploy"
                "cffi-libffi"
                "cl-glut"
                "cl-opengl"))

;; (sb-ext:save-lisp-and-die "blink"
;;                           :executable t
;;                           :compression t
;;                           :toplevel 'blink::main)

(defmacro bundle-libs (&rest libraries)
  `(progn ,@(loop for (lib path) in libraries
                  collect `(prog1 ,lib
                             (cffi:define-foreign-library ,(read-from-string lib)
                               (T (:default ,lib)))
                             (deploy:define-library ,(read-from-string lib)
                               :path ,path)))))


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

  (bundle-libs ("libX11"      "/opt/X11/lib/libX11.dylib")
               ("libX11-xcb"  "/opt/X11/lib/libX11-xcb.dylib")
               ("libXext"     "/opt/X11/lib/libXext.dylib")
               ("libglapi"    "/opt/X11/lib/libglapi.dylib")
               ("libxcb"      "/opt/X11/lib/libxcb.dylib")
               ("libXau"      "/opt/X11/lib/libXau.dylib")
               ("libxcb-glx"  "/opt/X11/lib/libxcb-glx.dylib"))

  (deploy::define-hook (:build sort-foreign-libraries) ()
    ;; Pretend to sort the list but this is just to ensure LIBSDL2 is loaded
    ;; before LIBSDL2-TTF and LIBSDL2-IMAGE
    (let ((sorted-libraries (reverse deploy::*foreign-libraries-to-reload*)))
      (setf deploy::*foreign-libraries-to-reload* sorted-libraries)
      (deploy::status 1 "Sorted libraries: ~A" sorted-libraries))))

(deploy:define-resource-directory assets "assets/")

(asdf:make :blink)
