(in-package #:blink)

(defclass tile (sprite)
  ((solid :initarg :solid :accessor solid :initform nil)
   (portal :initarg :portal :accessor portal :initform nil)))

(defun make-tile (tile-id x y spritesheet &key solid portal)
  (make-instance 'tile
                 :tex spritesheet
                 :rect (sdl2:make-rect (* tile-id 16) 0 16 16)
                 :x x :y y
                 :w +sprite-size+
                 :h +sprite-size+
                 :color +gray-40+
                 :solid solid
                 :portal portal))

;; (make-tile 2 64 64)

(defun add-tiles-to-scene (scene tiles spritesheet)
  (loop for row in tiles
        for y = 0 then (incf y +sprite-size+)
        do (loop for tile-id in row
                 for x = 0 then (incf x +sprite-size+)
                 do (cond
                      ((and (numberp tile-id)
                            (= 0 tile-id))
                       nil)
                      ((symbolp tile-id)
                       (add-to-scene scene (make-tile 0 x y spritesheet :portal tile-id)))
                      (t
                       (add-to-scene scene (make-tile tile-id x y spritesheet :solid t)))))))

;; (load-room *room*)
;; (remove-all-entities-from-scene (scene *window*))

(defun rect-at (x y)
  (make-rect (* +sprite-size+ (floor x +sprite-size+))
             (* +sprite-size+ (floor y +sprite-size+))
             +sprite-size+
             +sprite-size+))

(defun solidp (tiles x y)
  (let ((x-idx (floor x +sprite-size+))
        (y-idx (floor y +sprite-size+)))
    (if (and (<= 0 x-idx (length (first tiles)))
             (<= 0 y-idx (length tiles)))
        (case (nth x-idx (nth y-idx tiles))
          ((1 2) t)
          (otherwise nil))
        t)))

;; (solid +room-a+ 0 0)
;; (solid +room-a+ 63 64)
;; (solid +room-a+ 64 63)
;; (solid +room-a+ 64 64)

(defun portal-at (tiles x y)
  (let ((x-idx (floor x +sprite-size+))
        (y-idx (floor y +sprite-size+)))
    (when (and (<= 0 x-idx (length (first tiles)))
               (<= 0 y-idx (length tiles)))
      (let ((tile (nth x-idx (nth y-idx tiles))))
        (when (symbolp tile)
          tile)))))

;; (portal-at '((0 0 A)) 64 0)

(defun collide-with-tile-at-rect (tiles rect)
  (with-slots (x y w h) rect
    (loop :for (x y) :in `((,x          ,y)
                           (,(+ x w -1) ,y)
                           (,x          ,(+ y h -1))
                           (,(+ x w -1) ,(+ y h -1)))
          :when (solidp tiles x y)
            :collect (rect-at x y))))

;; (collide-with-tile-at-rect +room-a+ (make-sprite-rect 64 64))
;; (collide-with-tile-at-rect +room-a+ (make-sprite-rect 64 63))
;; (collide-with-tile-at-rect +room-a+ (make-sprite-rect 0 0))

(defun collide-with-portal (tiles rect)
  (with-slots (x y w h) rect
    (loop :for (x y) :in `((,x          ,y)
                           (,(+ x w -1) ,y)
                           (,x          ,(+ y h -1))
                           (,(+ x w -1) ,(+ y h -1)))
          :for portal-sym = (portal-at tiles x y)
          :when portal-sym
            :collect portal-sym)))

;; (collide-with-portal '((0 0 A)) (make-sprite-rect 64 0))
