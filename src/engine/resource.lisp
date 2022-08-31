(in-package #:blink)

(defvar *resources* (make-hash-table))

(defclass resource () ())

(defun load-resource (filename &rest all-keys &key type &allow-other-keys)
  (symbol-macrolet ((resource (gethash key *resources*)))
    (let ((key (alexandria:make-keyword
                (alexandria:symbolicate filename (format nil "~a" all-keys)))))
      (when (not resource)
        (setf resource
              (apply #'load-typed-resource
                     (list* filename type all-keys))))
      resource)))

(defgeneric load-typed-resource (filename type &key &allow-other-keys))

(defmethod load-typed-resource (filename type &key &allow-other-keys)
  (error (format nil "Unsupported resource type ~a" type)))

(defgeneric free-resource (resource))

(defmethod free-resource :around (resource)
  (when resource
    (call-next-method)))

(defun relative-path (path)
  (format nil "~a" (asdf:system-relative-pathname 'blink path)))
