(in-package #:blink)

(defvar *npcs* (make-hash-table))

(defclass npc (sprite animator interactable) ())

(defun make-npc (tex npc-id x y &key (w 64) (h 64) active-sprite interact-sprite)
  (let ((npc (make-instance 'npc
                            :tex tex
                            :rect (sdl2:make-rect 0 (* npc-id 16) 16 16)
                            :x x :y y :h h :w w
                            :current-animation :idle)))
    (when active-sprite
      (setf (active-sprite npc) active-sprite))
    (when interact-sprite
      (setf (interact-sprite npc) interact-sprite))
    (with-slots (animations) npc
      (setf (gethash :idle animations)
            (list (sdl2:make-rect 0 0 16 16)
                  (sdl2:make-rect 0 0 16 16)
                  (sdl2:make-rect 16 0 16 16)
                  (sdl2:make-rect 16 0 16 16)
                  (sdl2:make-rect 0 0 16 16)
                  (sdl2:make-rect 0 0 16 16)
                  (sdl2:make-rect 16 0 16 16)
                  (sdl2:make-rect 16 0 16 16))))
    (setf (gethash npc-id *npcs*) npc)
    npc))
