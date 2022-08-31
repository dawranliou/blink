(in-package #:blink)

(defclass tile (sprite)
  ((wallp :initarg :wallp :accessor wallp :initform nil)))

(defun make-tile (tile-id x y &key wallp)
  (make-instance 'tile
                 :tex *bg-tex*
                 :rect (sdl2:make-rect (* tile-id 16) 0 16 16)
                 :x x :y y
                 :w +sprite-size+
                 :h +sprite-size+
                 :color +gray-50+
                 :wallp wallp))

;; (make-tile 2 64 64)

(defun add-tiles-to-scene (scene tiles)
  (loop for row in tiles
        for y = 0 then (incf y +sprite-size+)
        do (loop for tile-id in row
                 for x = 0 then (incf x +sprite-size+)
                 when (not (zerop tile-id))
                   do (add-to-scene scene (make-tile tile-id x y :wallp t)))))

;; (load-room *room*)
;; (remove-all-entities-from-scene (scene *window*))
