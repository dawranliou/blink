;; Load game + engine
(ql:quickload "blink")

;; Start game window in the REPL
(run)

;; Resume sdl2 idle rendering loop
(setf (kit.sdl2:render-enabled *window*) t)

;; Debugging
(setf *debug-sprite* t)

;; Remove all entities
(remove-all-entities-from-scene (scene *window*))

;; Set player position
(setf (x *player*) 100 (y *player*) 100)

;; Add player back to scene
(progn
  (setf *player* (make-player *player-tex* (* 4 +sprite-size+) (* 10 +sprite-size+)))
  (add-to-scene (scene *window*) *player*))

;; Add tiles back to scene
(add-tiles-to-scene (scene *window*) +room-a+)

(transition-to-scene *window* (make-title-scene))
(transition-to-scene *window* (make-level 'A))
(transition-to-scene *window* (make-level 'C))
(transition-to-scene *window*
                     (make-level 'B
                                 :player-x +sprite-size+
                                 :player-y (* 14 +sprite-size+)))
