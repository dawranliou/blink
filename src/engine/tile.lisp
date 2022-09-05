(in-package #:blink)

(defclass tile (sprite)
  ((solidp :initarg :solidp :accessor solidp :initform nil)))

(defun make-tile (tile-id x y &key solidp)
  (make-instance 'tile
                 :tex *bg-tex*
                 :rect (sdl2:make-rect (* tile-id 16) 0 16 16)
                 :x x :y y
                 :w +sprite-size+
                 :h +sprite-size+
                 :color +gray-50+
                 :solidp solidp))

;; (make-tile 2 64 64)

(defun add-tiles-to-scene (scene tiles)
  (loop for row in tiles
        for y = 0 then (incf y +sprite-size+)
        do (loop for tile-id in row
                 for x = 0 then (incf x +sprite-size+)
                 when (not (zerop tile-id))
                 do (add-to-scene scene (make-tile tile-id x y :solidp t)))))

;; (load-room *room*)
;; (remove-all-entities-from-scene (scene *window*))

(defun rect-at (x y)
  (make-rect (* +sprite-size+ (floor x +sprite-size+))
             (* +sprite-size+ (floor y +sprite-size+))))

(defun solidp (tiles x y)
  (let ((x-idx (floor x +sprite-size+))
        (y-idx (floor y +sprite-size+)))
    (case (nth x-idx (nth y-idx tiles))
      ((1 2) t)
      (otherwise nil))))

;; (solidp *tiles* 0 0)
;; (solidp *tiles* 63 64)
;; (solidp *tiles* 64 63)
;; (solidp *tiles* 64 64)

(defun collide-with-tile (tiles rect)
  (with-slots (x y w h) rect
    (loop :for (x y) :in `((,x          ,y)
                           (,(+ x w -1) ,y)
                           (,x          ,(+ y h -1))
                           (,(+ x w -1) ,(+ y h -1)))
          :when (solidp tiles x y)
            :collect (rect-at x y))))

;; (collide-with-tile *tiles* (make-rect 64 64))
;; (collide-with-tile *tiles* (make-rect 64 63))
;; (collide-with-tile *tiles* (make-rect 0 0))
