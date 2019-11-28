;;;; Change Slot-definition-name and slot-value in clone-sprite to your implementation!

(in-package #:sdl-helper-functions)

;; Shapes
(defclass pos ()
  ((x :initarg :x :accessor x)
   (y :initarg :y :accessor y))
  (:documentation "Positions of an object, used as a super class for circle and rect"))

(defclass circle (pos)
  ((radius :initarg :r :accessor r :documentation "The circle's radius"))
  (:documentation "A cirlce"))

(defclass rect (pos)
  ((w :initarg :w :accessor w)
   (h :initarg :h :accessor h))
  (:documentation "A rectangle"))

;; Change to use a generic package instead
(defun clone-sprite (object &aux (copy (allocate-instance (class-of object))))
  "Clones an object."
  (loop for slot in (mapcar #'closer-mop:slot-definition-name
			    (closer-mop:class-slots (class-of object)))
			    ;(funcall class-slots (class-of object)))
     with value do ; holds the slot value from original object
       (when (slot-boundp object slot)
	 (setf value (slot-value object slot))
	 ;; Ensures a new array is created if the slot holds an array
	 (cond ((or (vectorp value) (arrayp value))
		(setf (slot-value copy slot) (make-array (length value)
							 :initial-contents value)))
	       ;; if value is a list, assume it's a list of instances, ie hitboxes
	       ((listp value)
		(let ((li))
		  (dolist (val value)
		    (when (typep val 'hitbox) ;; Ensures a new instance of hitbox objects
		      (push (clone-sprite val) li)))
		  
		  (setf (slot-value copy slot) li)))
	       
	       (t (setf (slot-value copy slot)
			(slot-value object slot))))))
  copy)



(defun set-x (object amount)
  "Set the x-position of the object(Rect\Circle class)"
  (setf (x object) amount))

(defun set-y (object amount)
  "Set the y-position of the object(Rect\Circle class)"
  (setf (y object) amount))

(defun incf-x (object amount)
  "increase the x-position of the object(Rect\Circle class)"
  (incf (x object) amount))

(defun incf-y (object amount)
  "increase the y-position of the object(Rect\Circle class)"
  (incf (y object) amount))



;;; Surface, each class that have surfaces have their own method in their respective source files
(defgeneric change-surface (object &key alpha)
  (:documentation "used to replace old surface for new parameters, 
such as changing the alpha of it, each specified type that uses surfaces have it's own method"))

(defmethod change-surface (object  &key alpha)
  (let* ((old-surface (get-surface object))
	 (surface (sdl:create-surface (sdl:width old-surface) (sdl:height old-surface))))
    (setf (get-surface object) surface)))
