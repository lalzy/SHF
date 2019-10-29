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

#||
(defclass circle ()
  ((position :initarg :position :accessor get-position :documentation "The cirlce's cordinate position as a vector")
   (radius :initarg :radius :accessor get-circle-radius :documentation "The circle's radius"))
  (:documentation "A cirlce"))

(defclass rect ()
  ((position :initarg :position :accessor get-position :documentation "The rectangle's cordinate position as a vector")
   (size :initarg :size :accessor get-rect-size :documentation "The rectangle's size as vector(width, height)"))
  (:documentation "A rectangle"))
||#

(defclass hitbox ()
  ((name :initarg :name :accessor get-hitbox-name :documentation "The hitbox name \ identifier")
   ;(rel-pos :initarg :rel-pos :accessor get-hitbox-rel-pos :documentation "Relative position to it's owner(such as a sprite)")
   (rel-x :initarg :rel-x :accessor get-hitbox-rel-x :documentation "Relative position to it's owner(such as a sprite)")
   (rel-y :initarg :rel-y :accessor get-hitbox-rel-y :documentation "Relative position to it's owner(such as a sprite)")
   (color :initarg :color :accessor get-box-color :documentation "The color of the hitbox, used when\if drawing them"))
  (:documentation "The hitbox super class. Hitboxes are used for the collision detection"))


(defclass hitbox-rect (rect hitbox)
  ()
  (:documentation "A rectangle hitbox. Used for collision detection"))

(defclass hitbox-circle (circle hitbox)
  ()
  (:documentation "A circle hitbox. Used for collision detection"))

(defun clone-sprite (object &aux (copy (allocate-instance (class-of object))) 
                                 (slot-definition-name #+:ccl #'ccl:slot-definition-name
													   #+:sbcl #'sb-mop:slot-definition-name)
								 (class-slots #+:ccl #'ccl:class-slots
								              #+sbcl #'sb-mop:class-slots))
  "Clones an object."
  (loop for slot in (mapcar slot-definition-name (funcall class-slots (class-of object)))
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

;; getters
;(defun x (object)
;  (format t "yo!"))

#||
;; setters
(defun set-x (object amount)
  "Set the x-position of the object(Rect\Circle class)"
  (setf (elt (get-position object) 0) amount))

(defun set-y (object amount)
  "Set the y-position of the object(Rect\Circle class)"
  (setf (elt (get-position object) 1) amount))

(defun incf-x (object amount)
  "increase the x-position of the object(Rect\Circle class)"
  (incf (elt (get-position object) 0) amount))

(defun incf-y (object amount)
  "increase the y-position of the object(Rect\Circle class)"
  (incf (elt (get-position object) 1) amount))
||#


#||
(defun get-object-size-or-position (type object &aux (vect object))
  "Get the value out of a vector's position"
  ;; Changes vect into relevant vector
  (cond ((and (or (string= type 'w) (string= type 'h)) (typep object 'rect))
	 (setf vect (get-rect-size object))) ; Gets rectangle size if object is a rectangle, and we want W\H
	((or (typep object 'rect) (typep object 'circle))
	 (setf vect (get-position object)))) ; Gets position vector if object is either circle or rectangle

  ;; Gets the value from correct vector position based on what we want
  (cond ((or (string= type 'x) (string= type 'w))
	 (elt vect 0))
	((or (string= type 'y) (string= type 'h))
	 (elt vect 1))))
  
;; getters
(defun x (object)
  (get-object-size-or-position 'x object))

(defun y (object)
  (get-object-size-or-position 'y object))

(defun w (object)
  (get-object-size-or-position 'w object))

(defun h (object)
  (get-object-size-or-position 'h object))


(defun r (circle)
  "Get the radius of the circle class object"
  (get-circle-radius circle))
||#
