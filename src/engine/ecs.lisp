(in-package #:blink)

(defclass entity ()
  ((id :accessor id :initarg :id)))

(defclass component () ())

(defclass system () ())

(defgeneric update (system entity-manager)
  (:documentation "Send updates to all entities in the entity-manager"))

(defclass entity-manager ()
  ((%entity-id-counter :initform 0)
   (entities :accessor entities :initform '())
   (components :accessor components :initform (make-hash-table))))

(defun get-component-entities (entity-manager component-sym)
  (or (gethash component-sym (components entity-manager))
      (setf (gethash component-sym (components entity-manager))
            (make-hash-table))))

(defun add-component (entity-manager entity component)
  (let* ((component-sym (type-of component))
         (component-entities (get-component-entities entity-manager
                                                     component-sym)))
    (push component (gethash entity component-entities))))

(defun %make-entity (entity-manager)
  (with-slots ((entity %entity-id-counter) entities) entity-manager
    (pushnew entity entities)
    (incf entity)
    entity))

(defun create-entity (entity-manager &rest components)
  (let ((entity (%make-entity entity-manager)))
    (dolist (component components)
      (add-component entity-manager entity component))
    entity))
