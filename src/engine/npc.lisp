(in-package #:blink)

(defvar *npcs* (make-hash-table))

(defclass npc (sprite animator) ())

(defun make-npc (tex npc-id x y &key (w 64) (h 64))
  (let ((npc (make-instance 'npc
                            :tex tex
                            :rect (sdl2:make-rect 0 (* npc-id 16) 16 16)
                            :x x :y y :h h :w w
                            :current-animation :idle)))
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