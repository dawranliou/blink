(in-package #:blink)

(defun add-background-game-object (id x y)
  (make-entity
   (list (make-box-component x y
                             +sprite-size+
                             +sprite-size+
                             +gray-50+)
         (make-sprite-component *bg-tex*
                                (sdl2:make-rect (* id 16) 0 16 16)
                                x y
                                +sprite-size+
                                +sprite-size+
                                :tint +gray-50+))))

;; (add-background-game-object 2 64 64)

(defun load-room (room-data)
  (loop for row in room-data
        for y = 0 then (incf y +sprite-size+)
        do (loop for id in row
                 for x = 0 then (incf x +sprite-size+)
                 when (not (zerop id))
                 do (add-background-game-object id x y))))

;; (load-room *room*)
