(in-package #:blink)

(defvar *resources*)

(defmacro with-resource-pool (resource-pool &body body)
  `(let ((*resources* ,resource-pool))
     ,@body))

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

(defun free-all-resources ()
  (loop for resource being the hash-values of *resources*
        do (free-resource resource))
  (clrhash *resources*))

(defun relative-path (path)
  (format nil "~a" (asdf:system-relative-pathname 'blink path)))
