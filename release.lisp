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

(deploy:define-resource-directory assets "assets/")

(asdf:make :blink)
