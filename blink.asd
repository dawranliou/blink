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
  :depends-on (#:sdl2kit #:sdl2-image #:sdl2-ttf)
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:module "engine"
                :components ((:file "color")
                             (:file "resource")
                             (:file "texture")
                             (:file "font")
                             (:file "camera")
                             (:file "renderer")
                             (:file "interactor")
                             (:file "scene")
                             (:file "entity")
                             (:file "sprite")
                             (:file "tile")
                             (:file "animator")
                             (:file "player")
                             (:file "npc")
                             (:file "physics")
                             (:file "window")))
               (:file "map")
               (:file "main")))
