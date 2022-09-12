(ql:quickload "deploy")

;; (sb-ext:save-lisp-and-die "blink"
;;                           :executable t
;;                           :compression t
;;                           :toplevel 'blink::main)

(deploy:define-resource-directory assets "assets/")

(asdf:make :blink)
