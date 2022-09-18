(in-package #:blink)

(defgeneric render (renderer thing &key camera &allow-other-keys)
  (:method (renderer thing &key camera &allow-other-keys)
    nil))

(defun run-renderer-system (renderer entities &key camera w h resource-pool)
  (loop :for entity :in entities
        :do (render renderer entity :camera camera
                                    :w w :h h
                                    :resource-pool resource-pool)))
