;;;; Change Slot-definition-name and slot-value in clone-sprite to your implementation!

(in-package #:sdl-helper-functions)

(defparameter *colors* `((white ,(sdl:color :r 255 :g 255 :b 255))
			 (black ,(sdl:color :r 0 :g 0 :b 0))
			 (darkgray ,(sdl:color :r 50 :g 50 :b 50))
			 (gray ,(sdl:color :r 160 :g 160 :b  160))
			 (lightgray ,(sdl:color :r 211 :g 211 :b 211))
			 (green ,(sdl:color :r 0 :g 255 :b 0))
			 (red ,(sdl:color :r 255 :g 0 :b 0))
			 (blue ,(sdl:color :r 0 :g 0 :b 255))
			 (cyan ,(sdl:color :r 0 :g 255 :b 255))
			 (yellow ,(sdl:color :r 255 :g 255 :b 0))))

;; Colors
(defmacro add-color (color &key (r 0) (g 0) (b 0))
  "Add a color to the *colors* list"
  `(push (list ',color (sdl:color :r ,r :g ,g :b ,b)) ,*colors*))

(defun find-color (color)
  "helper function for get-color"
  (cadr (assoc color *colors* :test #'string=)))

(defmacro get-color (color)
  "Returns a chosen color from the list of SDL colors found in *colors*"
  `(find-color ',color))

(defun get-rgb (&key (r 0) (g 0) (b 0))
  "Get an SDL color object from passed R,G,B"
  (sdl:color :r r :g g :b b))


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


;; Change to use a generic package instead
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


