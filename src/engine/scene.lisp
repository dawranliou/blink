(in-package #:blink)

(defclass scene ()
  ((bg :accessor bg :initform +black+ :initarg :bg)
   (camera :accessor camera :initarg :camera)
   (game-objects :accessor game-objects :initarg :game-objects)
   (rooms :accessor rooms)))
