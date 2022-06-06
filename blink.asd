;;;; blink.asd
(in-package #:cl-user)

(defpackage #:blink-asd
  (:use :cl :asdf))

(in-package #:blink-asd)

(asdf:defsystem #:blink
  :description "Describe blink here"
  :author "Daw-Ran Liou <hi@dawranliou.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :defsystem-depends-on (:deploy)
  :build-operation #+linux "deploy-op"
                   #+darwin "osx-app-deploy-op"
  :build-pathname "blink"
  :entry-point "blink:main"
  :depends-on (#:cl-raylib)
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "blink")))
