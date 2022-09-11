(in-package #:blink)

(defclass animator (texture)
  ((timer :accessor timer :initform 0)
   (frame :accessor frame :initform 0)
   (current-animation :accessor current-animation :initarg :current-animation)
   (animations :accessor animations :initform (make-hash-table))))

(defun get-current-animation-rect (animator animation-key)
  (with-slots (frame animations) animator
    (let ((animation-series (gethash animation-key animations)))
      (when animation-series
        (nth frame animation-series)))))

(defun tick-animator (animator dt)
  (incf (timer animator) dt)
  (when (<= 250 (timer animator))
    (setf (timer animator) 0)
    (incf (frame animator))
    (when (<= 8 (frame animator))
      (setf (frame animator) 0))
    (setf (rect animator)
          (get-current-animation-rect animator (current-animation animator)))))

(defun run-animator-system (entities dt)
  (loop :for entity :in entities
        :when (typep entity 'animator)
        :do (tick-animator entity dt)))

(defun set-animation (animator animation-key)
  (unless (eql animation-key (current-animation animator))
    (setf (timer animator) 0
          (frame animator) 0
          (rect animator) (get-current-animation-rect animator animation-key)))
  (setf (current-animation animator) animation-key))
