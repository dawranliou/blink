(in-package #:blink)

(defun rgb (red green blue &optional (alpha 1.0))
  (let ((alpha* (round (* alpha 255))))
    `(,red ,green ,blue ,alpha*)))

(defun gray (percentage &optional (alpha 1.0))
  (let ((amount (round (* percentage 255 1/100))))
    (rgb amount amount amount alpha)))

(defparameter +black+ (gray 0))
(defparameter +gray-40+ (gray 40))
(defparameter +gray-50+ (gray 50))
(defparameter +white+ (gray 100))
