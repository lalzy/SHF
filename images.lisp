(in-package :sdl-helper-functions)

;;; Create images \ Sprite sheets
(defun make-image (image &key (path "") color-key-pos)
  "Loads up an image from path and returns it as a surface"
  (shf-error:try-retry
   (sdl:load-and-convert-image (merge-pathnames path image) :color-key-at color-key-pos)
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


(defun generate-sheet-cells (cell-width sprite-width &key (cell-height cell-width) (sprite-height sprite-width))
  "Generate the sprite-cell list"
  (iter (for y from 0 to cell-height by sprite-height)
	(appending (iter (for x from 0 to cell-width by sprite-width)
			 (collect (list x y sprite-width sprite-height))))))
#||
  (loop for y from 0 to cell-height by sprite-height append
       (loop for x from 0 to cell-width by sprite-width collect (list x y sprite-width sprite-height))))
||#
(defun make-sprite-sheet (image cells &key (color-key-pos))
  "Returns a list of sprite surfaces cut by cells(Sequence) ex:
   ((0 0 32 32) (0 32 32 32) (0 64 32 32))"
  (loop for cell in cells collect (sub-image image cell color-key-pos)))



;; SPrites - to be changed
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
   (sprite-set
    :initarg :animations
    :accessor get-sprite-set
    :documentation "All the sprite sets. 
Animations are a sequence of sprite sheet surfaces created through make-sprite-sheet function."))
  (:documentation "Super class for all sprites."))

;;; Create sprite object

(defun split-image (image cells color-key-pos)
  (if cells
      (make-sprite-sheet image cells :color-key-pos color-key-pos)
      (list image)))

(defun create-sprite (image-name &key cells (x 0) (y 0) (path #p"") color-key-pos)
  "Creates a sprite from image(set)"
  (let* ((image (split-image (make-image image-name :path (merge-pathnames path image-name) :color-key-pos color-key-pos) cells color-key-pos))
	 (sprite (make-instance 'sprite-class :sprite (elt image 0)
				:x x :y y :w (sdl:width (elt image 0))  :h (sdl:height (elt image 0)))))
    (push sprite *sprite-group*)
    sprite))


;;; Simple geometric sprites


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


;;; Drawing and releated functions

(defun draw-sprites ()
  "Goes through the global variable *sprite-group*, 
which is a list of all sprites that should be automatically drawn"
  (dolist (sprite *sprite-group*)
    ;; Only draws when visible on screen and it's a sprite
    (and (not (edge-collision-check sprite t)) (equal (type-of sprite) 'sprite-class)
      (sdl:draw-surface-at-* (get-sprite sprite) (x sprite) (y sprite)))))

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




;;; Moving\positioning Sprite objects and related hitboxes


(defun move-sprite (sprite dir &optional (amount 0))
  "Moves a sprite and all it's hitboxes by x-amount in decided direction(:horizontal or :vertical)"
  (when (string= dir :horizontal)
    (dolist (hitbox (get-sprite-hitboxes sprite))
      (incf-x  hitbox amount))
    (incf-x  sprite amount))
  
  (when (string= dir :vertical)
      (dolist (hitbox (get-sprite-hitboxes sprite))
	(incf-y  hitbox amount))
      (incf-y sprite amount)))

(defun set-sprite-pos (sprite &key (x nil) (y nil))
  "Sets the absolute X and\or Y cordinate position of the sprite"
  (when x
    (set-x sprite x)
    (dolist (hitbox (get-sprite-hitboxes sprite))

      (set-x hitbox (+ x (get-hitbox-rel-x hitbox)))))
  
  (when y
    (set-y sprite y)
    (dolist (hitbox (get-sprite-hitboxes sprite))
      (set-y hitbox (+ y (get-hitbox-rel-y hitbox))))))
