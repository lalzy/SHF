(in-package #:sdl-helper-functions)

(defparameter *sprite-group* nil) ; Group of sprites to be drawn
(defparameter *hitbox-draw-group* nil) ; Group of hitboxes to be drawn(if in debug)

;; Sprite class
(defclass sprite-class (rect) 
  ((sprite
    :initarg :sprite
    :accessor get-sprite
    :documentation "The image surface that will be drawn(automatically)")
   (hitboxes ; List of hitboxes, which are rect or circle class
    :initform nil
    :accessor get-sprite-hitboxes
    :documentation "A sequence of hitboxes(instances of a hitbox shape class) owned\used for this sprite")
   (animation-sets
    :initarg :animations
    :accessor get-sprite-animations
    :documentation "A sequence of animations. 
Animations are a sequence of sprite sheet surfaces created through make-sprite-sheet function."))
  (:documentation "Super class for all sprites."))

(defun empty-sprite-group ()
  "Clears the sprite-group"
  (setf *sprite-group* nil)
  (setf *hitbox-draw-group* nil))

(defun delete-from-sprite-group (sprite)
  "Removes selected sprite from the sprite-group"
  (format t "sprite-group = ~{~a, ~}~%" *sprite-group*)
  (setf *sprite-group* (remove sprite *sprite-group*)))

(defun add-to-sprite-group (sprite)
  "Adds selected sprite to the sprite-group"
  (unless (member sprite *Sprite-group*)
    (push sprite *sprite-group*)))

;;; Drawing

(defun draw-sprites ()
  "Goes through the global variable *sprite-group*, 
which is a list of all sprites that should be automatically drawn"
  (dolist (sprite *sprite-group*)
    ;; Only draws when visible on screen and it's a sprite
    (and (not (edge-collision-check sprite t)) (equal (type-of sprite) 'sprite-class)
      (sdl:draw-surface-at-* (get-sprite sprite) (x sprite) (y sprite)))))

(defun draw-hitboxes ()
  "Goes through the global variable *hitbox-draw-group*, 
which is a list of hitboxes that shoyld be automatically drawn"
  (when *debug-hitbox-draw* ; Checks if hitboxes are to be shown or not
    (dolist (hitbox *hitbox-draw-group*)
      (let ((x (x hitbox))
	    (y (y hitbox))
	    (color (get-box-color hitbox)))
	;; Only draws hitboxes when on screen
	(unless (edge-collision-check hitbox  t)
	  (when (equalp (type-of hitbox) 'hitbox-rect)
	    (sdl:draw-rectangle-* x y (w hitbox) (h hitbox) :color color))
	  (when (equalp (type-of hitbox) 'hitbox-circle)
	    (sdl:draw-circle-* x y (r hitbox) :color color)))))))

;;; Moving\positioning Sprite objects and related hitboxes
(defun move-sprite (sprite dir &optional (amount 0))
  "Moves a sprite and all it's hitboxes by x-amount in decided direction(:horizontal or :vertical)"
  (when (equalp dir :vertical)
    (dolist (hitbox (get-sprite-hitboxes sprite))
      (incf-x  hitbox amount))
    (incf-x  sprite amount))
  
  (when (equalp dir :horizontal)
      (dolist (hitbox (get-sprite-hitboxes sprite))
	(incf-y  hitbox amount))
      (incf-y sprite amount)))

(defun set-sprite-pos (sprite &key (x nil) (y nil))
  "Sets the absolute X and\or Y cordinate position of the sprite"
  (when x
    (set-x sprite x)
    (dolist (hitbox (get-sprite-hitboxes sprite))
      ;(set-x hitbox (+ x (elt (get-hitbox-rel-pos hitbox) 0)))))
      (set-x hitbox (+ x (get-hitbox-rel-x hitbox)))))
  
  (when y
    (set-y sprite y)
    (dolist (hitbox (get-sprite-hitboxes sprite))
     ; (set-y hitbox (+ y (elt (get-hitbox-rel-pos hitbox) 1))))))
      (set-y hitbox (+ y (get-hitbox-rel-y hitbox))))))

;;; Create images \ Sprite sheets
(defun make-image (image-path &key (color-key-pos))
  "Loads up an image from path and returns it as a surface"
  (shf-error:try-retry
   (let* ((image (sdl:load-image image-path :color-key-at color-key-pos))
	  (surface (sdl:create-surface (sdl:width image) (sdl:height image) :color-key-at color-key-pos)))
     (sdl:draw-surface image :surface surface)
     surface)
   :title "Cannot load image error"))

(defun sub-image (image cell color-key-pos)
  "Subsects image cells into seperate surfaces"
  (let* ((x (elt cell 0))
	 (y (elt cell 1))
	 (w (elt cell 2))
	 (h (elt cell 3))
	 (new-surface (sdl:create-surface w h :color-key-at color-key-pos)))
  (setf (sdl:cells image) (sdl:rectangle :x x :y y :h h :w w))
  (sdl:draw-surface image :surface new-surface)
  new-surface))


(defun make-sprite-sheet (image-path cells &key (color-key-pos))
  "Returns a list of sprite surfaces cut by cells(Sequence) ex:
   ((0 0 32 32) (0 32 32 32) (0 64 32 32))"
  (shf-error:try-retry
   (let* ((image (sdl:load-image image-path :color-key-at color-key-pos))
	  (sprites (loop for cell in cells collect (sub-image image cell color-key-pos))))
     sprites)
   :title "Cannot load image error"))

;;; Hitbox and sprite-shape Constructors(Box\Circle) and helper functions

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
    
    (push hitbox *hitbox-draw-group*) ; Add hitbox to hitbox draw-group for drawing
    hitbox))
  


(defun make-circle-sprite (r color &key (x 0) (y 0) &aux
			     (w (1+ (* r 2)))
			     (h  (1+ (* r 2))))
  "Creates a circle sprite of r radius"
  (let* ((new-surface (sdl:create-surface w h :color-key-at #(0 0)))
	 (sprite (make-instance 'sprite-class
				:sprite new-surface
				:x (+ x r)
				:y (+ y r)
				:w w
				:h h)))
    (sdl:draw-filled-circle-* r r r :surface new-surface :color color)
    (push sprite *sprite-group*)
    sprite))


(defun make-box-sprite (w h color &key (x 0) (y 0))
  "Creates a box sprite of W\H size"
  (let* ((new-surface (sdl:create-surface w h))
	(sprite (make-instance 'sprite-class
			       :sprite new-surface
			       :x x :y y :w w :h h)))
    (sdl:draw-box-* 0 0 w h :surface new-surface :color color)
    (push sprite *sprite-group*)
    sprite))


#||
(defun create-hitbox (type &key (sprite nil)
			     (x nil) (y nil) (w nil) (h nil) (r nil) (name "") (color (get-color blue)))
  "Creates a hitbox of [type](rect or circle) that may or may not be linked to a sprite"
  (let* ((hitbox nil)
	 (position (create-hitbox-position type sprite x y))
	 (size (create-hitbox-size type sprite w h r)))
    
    (if (string= type 'rect) ; Decides what kind of hitbox to make
	(setf hitbox (make-instance 'hitbox-rect :position position :size size :name name :color color))
	(setf hitbox (make-instance 'hitbox-circle :position position :radius size :name name :color color)))
    
    (when sprite ; Add hitbox to group of hitboxes
      (setf (get-hitbox-rel-pos hitbox) (create-hitbox-relative-position type x y (w sprite) (h sprite)))
      (setf (get-sprite-hitboxes sprite) (push hitbox (get-sprite-hitboxes sprite))))
    
    (push hitbox *hitbox-draw-group*) ; Add hitbox to hitbox draw-group for drawing
    hitbox))
  


(defun make-circle-sprite (r color &key (x 0) (y 0)
			   &aux
			     (w (1+ (* r 2)))
			     (h  (1+ (* r 2)))
			     (new-surface (sdl:create-surface w h :color-key-at #(0 0)))
			     (sprite (make-instance 'sprite-class
						    :sprite new-surface
						    :position (vector (+ x r) (+ y r)) :size (vector w h))))
  "Creates a circle sprite of r radius"
  +
  +(sdl:draw-filled-circle-* r r r :surface new-surface :color color)
  (push sprite *sprite-group*)
  sprite)


(defun make-box-sprite (w h color &key (x 0) (y 0)
			&aux
			  (new-surface (sdl:create-surface w h))
			  (sprite (make-instance 'sprite-class
						 :sprite new-surface
						 :position (vector x y) :size (vector w h))))
  "Creates a box sprite of W\H size"
  (sdl:draw-box-* 0 0 w h :surface new-surface :color color)
  (push sprite *sprite-group*)
  sprite)
||#