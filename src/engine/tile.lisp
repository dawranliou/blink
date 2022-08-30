(in-package #:blink)

(defclass tile (sprite) ())

(defun make-tile (tile-id x y)
  (make-sprite *bg-tex*
               (sdl2:make-rect (* tile-id 16) 0 16 16)
               x y
               +sprite-size+
               +sprite-size+
               :color +gray-50+))

;; (make-tile 2 64 64)

(defun add-room-to-scene (scene room-data)
  (loop for row in room-data
        for y = 0 then (incf y +sprite-size+)
        do (loop for tile-id in row
                 for x = 0 then (incf x +sprite-size+)
                 when (not (zerop tile-id))
                   do (add-to-scene scene (make-tile tile-id x y)))))

;; (load-room *room*)
;; (remove-all-entities-from-scene (scene *window*))
