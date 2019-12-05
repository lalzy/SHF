(in-package #:sdl-helper-functions)
(defparameter *hitbox-draw-group* nil) ; Group of hitboxes to be drawn(if in debug)

(defclass hitbox ()
  ((name :initarg :name :accessor get-hitbox-name :documentation "The hitbox name \ identifier")
   ;(rel-pos :initarg :rel-pos :accessor get-hitbox-rel-pos :documentation "Relative position to it's owner(such as a sprite)")
   (rel-x :initarg :rel-x :accessor get-hitbox-rel-x :documentation "Relative position to it's owner(such as a sprite)")
   (rel-y :initarg :rel-y :accessor get-hitbox-rel-y :documentation "Relative position to it's owner(such as a sprite)")
   (color :initarg :color :accessor get-box-color :documentation "The color of the hitbox, used when\if drawing them"))
  (:documentation "The hitbox super class. Hitboxes are used for the collision detection"))

(defclass hitbox-rect (rect hitbox)
  () (:documentation "A rectangle hitbox. Used for collision detection"))

(defclass hitbox-circle (circle hitbox)
  () (:documentation "A circle hitbox. Used for collision detection"))


(defun draw-hitboxes ()
  "Goes through the global variable *hitbox-draw-group*, 
which is a list of hitboxes that shoyld be automatically drawn"
  ;(when *debug-hitbox-draw* ; Checks if hitboxes are to be shown or not
    (dolist (hitbox *hitbox-draw-group*)
      (let ((x (x hitbox))
	    (y (y hitbox))
	    (color (get-box-color hitbox)))
	;; Only draws hitboxes when on screen
	(unless (edge-collision-check hitbox  t)
	  (when (equalp (type-of hitbox) 'hitbox-rect)
	    (sdl:draw-rectangle-* x y (w hitbox) (h hitbox) :color color))
	  (when (equalp (type-of hitbox) 'hitbox-circle)
	    (sdl:draw-circle-* x y (r hitbox) :color color))))));)


(defun create-hitbox-position (type sprite x y )
  "Creates the position of the hitbox in relation to the sprites position(if a sprite is provided).
Also defaults the hitbox positions to be the center for cirlce and top-left for rectangles(if no position is given)."
  (if sprite
      ;; Sets the X\Y position to upper left corner of sprite if a rect,
      ;;  or center if a circle
    (cond ((string= type 'rect)
	   (when (null x) (setf x 0))
	   (when (null y) (setf y 0))
	   (incf x (x sprite))
	   (incf y (y sprite)))
	  ((string= type 'circle)
	   (if (null x)
	       (setf x (+ (x sprite) (round (/ (w sprite) 2))))
	       (incf x (x sprite)))
	   (if (null y)
	       (setf y (+ (y sprite) (round (/ (h sprite) 2))))
	       (incf y (y sprite)))))
    (progn (when (null x) (setf x 0))
	   (when (null y) (setf y 0))))
  (vector x y))

(defun create-hitbox-relative-position (type x y w h)
  "Creates the position of the hitbox in relation to the sprites position(if a sprite is provided).
Also defaults the hitbox positions to be the center for cirlce and top-left for rectangles(if no position is given)."
      ;; Sets the X\Y position to upper left corner of sprite if a rect,
      ;;  or center if a circle
  (cond ((string= type 'rect)
	 (when (null x) (setf x 0))
	 (when (null y) (setf y 0)))
	((string= type 'circle)
	 (when (null x)
	   (setf x (round (/ w 2))))
	 (when (null y)
	   (setf y (round (/ h 2))))))
  (vector x y))

(defun create-hitbox-size (type sprite w h r)
  "Creates the hitbox size based on type and sprite. 
If no parameters made by default w\h will be size of sprite, in case of circle, radius will cover entire sprite.
If no sprite is provided, it will check type, if it's a rect a size vector will be returned, if not radius is returned"
  (if sprite
      ;;
      (cond ((string= type 'rect)
	     (when (null w) (setf w (w sprite)))
	     (when (null h) (setf h (h sprite))))
	    ((string= type 'circle)
	     (when (null r) (setf r (w sprite)))))
      (progn
	(when (null w) (setf w 5))
	(when (null h) (setf h 5))
	(when (null r) (setf r 5))))
  (if (string= type 'rect)
      (vector w h)
      r))


(defun create-hitbox (type &key (sprite nil)
			     (x nil) (y nil) (w nil) (h nil) (r nil) (name "") (color (get-color blue)))
  "Creates a hitbox of [type](rect or circle) that may or may not be linked to a sprite"
  (let* ((hitbox nil)
	 (position (create-hitbox-position type sprite x y))
	 (size (create-hitbox-size type sprite w h r))
	 (nx (elt position 0))
	 (ny (elt position 1))
	 (nw nil)
	 (nh nil))
  
  (when (string= type 'rect)
    (setf nw (elt size 0)
	  nh (elt size 1)))
  
  (if (string= type 'rect) ; Decides what kind of hitbox to make
      (setf hitbox (make-instance 'hitbox-rect :x nx :y ny :w nw :h nh :name name :color color))
     ; (setf hitbox (make-instance 'hitbox-rect :x nx :y ny :w nw :h nh  :name name :color color))
      (setf hitbox (make-instance 'hitbox-circle :x nx :y ny :r size :name name :color color )))
      ;(setf hitbox (make-instance 'hitbox-circle :x nx :y ny :radius size :name name :color color)))
    
    (when sprite ; Add hitbox to group of hitboxes
      ;(setf (get-hitbox-rel-pos hitbox) (create-hitbox-relative-position type x y (w sprite) (h sprite)))
      (let* ((pos (create-hitbox-relative-position type x y (w sprite) (h sprite)))
	     (px (elt pos 0))
	     (py (elt pos 1)))
	(setf (get-hitbox-rel-x hitbox) px)
	(setf (get-hitbox-rel-y hitbox) py))
      (setf (get-sprite-hitboxes sprite) (push hitbox (get-sprite-hitboxes sprite))))
    
    (when color ; Add hitbox to hitbox draw-group for drawing only if a color has been supplied
      (push hitbox *hitbox-draw-group*)) 
    hitbox))
  
