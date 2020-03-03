(in-package :sdl-helper-functions)

(defun 1a (array)
  (aref array 0))

(defun 2a (array)
  (aref array 1))


(defun initialize-context-menu ()
  ;; Create context-menu state here!
  (add-state :context-menu)
  (let (menu-items menu-pos (height 0) background (width 0) previous-state key-mode (browse-keys (make-hash-table :size 4))
		   (selection-index 0) (item-amount 0))
    
    (defun clear-context-menu (&optional item)
      (setf menu-items nil   selection-index 0   item-amount 0
	    height 0         width 0             *state* previous-state
	    background nil)
      (when item item))
    
    (defun set-browse-keys (down up select cancel)
      (setf (gethash :down browse-keys) down
	    (gethash :up browse-keys) up
	    (gethash :select browse-keys) select
	    (gethash :cancel browse-keys) cancel
	    key-mode t))

    (defun set-context-menu-size (string spacing)
      (when (> (w string) width) (setf width (w string)))
      (incf height (+ spacing (h string))))
    
    (defun create-context-menu (string-list &key (one-instance t) (spacing 0) (x (sdl:mouse-x)) (offset-y 0) (offset-x 0) (y (sdl:mouse-y)) (font sdl:*default-font*))
      (when (and one-instance (> (length menu-items) 0))
	(clear-context-menu))

      (unless (string= *state* :context-menu)
	(setf previous-state *state*))
      (set-state :context-menu)
      (setf menu-pos (vector x y))
      (incf x offset-x)
      (incf y offset-y)
      
      (dolist (string string-list)
	(incf item-amount)
	(push (vector string
		      (vector x y (w string font) (h string font))) menu-items)

	;; +1 to prevent multi-selection with mouse
	(set-context-menu-size string (+ 1 spacing))
	
	(incf y (+ 1 spacing (h string font))))
      
      (if key-mode
	  (setf selection-index (1- item-amount))
	  (setf selection-index -1)))
    
    (defun move-index (check-function check-value reset amount)
      (if (funcall check-function selection-index check-value)
	  (setf selection-index reset)
	  (setf selection-index (+ selection-index amount))))

    (defun mouse-check ()
      (dolist (item menu-items)
	(when (mouse-collision-check (2a item)) (return-from mouse-check (1a item))))
      nil)

    (defun mouse-select-context-item (&key (select-keys '(:left)) cancel-keys)
      (cond ((is-mouse-keys cancel-keys)
	     (clear-context-menu))
	    ((is-mouse-keys select-keys)
	       (clear-context-menu (mouse-check)))))

    (defun key-select-context-item (&aux (key (shf:get-pressed-key)))
      (cond ((string= (gethash :cancel browse-keys) key)
	     (clear-context-menu))
	    ((string= key (gethash :up browse-keys))
	     (move-index #'>= (1- item-amount) 0 1)
	     nil)
	    ((string= key (gethash :down browse-keys))
	     (move-index #'<= 0 (1- item-amount) -1)
	     nil)
	    ((string= key (gethash :select browse-keys))
	     (clear-context-menu (1a (nth selection-index menu-items))))))
	    

    (defun create-color-box (&optional (color (get-color light-gray)) (extra-width 0) (extra-height 0))
      (let ((surface (sdl:create-surface (+ width extra-width) (+ height extra-height))))
	(sdl:draw-box-* 0 0 (+ width extra-width) (+ height extra-height) :color color :surface surface)
	surface))

    ;; Create a better background system.
    
    (defun run-context-menu (&key (text-color (get-color black)) (select-text-color (get-color blue))
			       ;(background (create-gray-box))
			       (mouse-on t))


     ; (shf:draw-text (strarg "size = ~a" *temporary*) #(0 120))
      
      ;; Add check if colliding with menu as well.
      (when (and mouse-on (mouse-moved)) (setf selection-index -1))
      (let ((index 0))
	(draw-context-background)
	(dolist (item menu-items)
	  ;; Change selection if mouse move and mouse is on
	  (draw-context-menu-item item index text-color select-text-color)
	  (incf index))))
    
    (defun draw-context-menu-item (item index text-color select-text-color)
      (cond ((= index selection-index)
	     (shf:draw-text (2a item) (1a item) :default-color select-text-color))
	    ((and (mouse-collision-check (2a item)) (= selection-index -1))
	     (shf:draw-text (2a item) (1a item) :default-color select-text-color))
	    (t
	     (shf:draw-text (2a item) (1a item) :default-color text-color))))


    (defun draw-context-background ()
      (unless background (setf background (vector (create-color-box) menu-pos)))

      (sdl:draw-surface-at (1a background) (2a background)))
    
    ;; Background creation
    (defun build-vertical-sides (image target-size color-key-pos)
      (let* ((max-size (round  target-size (h image) ))
	     (amount 0)
	     (surface (sdl:create-surface (w image) (* (h image) max-size) :color-key-at color-key-pos)))
	(loop for i from 0 to max-size with y = 0 do
	     (sdl:draw-surface-at-* image 0 y :surface surface)
	     (incf amount)
	     (incf y (h image)))
	surface))

    (defun build-horizontal-sides (image target-size color-key-pos)
      (let* ((max-size (round  target-size (w image) ))
	     (amount 0)
	     (surface (sdl:create-surface (* (w image) max-size) (h image) :color-key-at color-key-pos)))
	(loop for i from 0 to max-size with x = 0 do
	     (sdl:draw-surface-at-* image x 0 :surface surface)
	     (incf amount)
	     (incf x (w image)))
	surface))

    ;; Create the ability to set an already build background
    (defun set-context-background ())
    
    (defmethod build-context-background ((background-texture sdl:color)  &key (color-key-pos #(0 0))  horizontal-image vertical-image corner-image)
      (let ((border-width (if vertical-image (* (w vertical-image) 2) 0))
	    (border-height (if horizontal-image (* (h horizontal-image) 2) 0)))
	(build-context-background-helper (create-color-box background-texture border-width border-height) color-key-pos  horizontal-image vertical-image corner-image)))
    
    (defmethod build-context-background ((background-texture sdl:surface) &key (color-key-pos #(0 0))  horizontal-image vertical-image corner-image)
      (build-context-background-helper background-texture color-key-pos  horizontal-image vertical-image corner-image))

    ;; Make support for drawing beyond scope for incomplete side
    (defun build-context-background-helper (background-texture color-key-pos  horizontal-image vertical-image corner-image)
      "Builds a background with an border image"
      (let* ((border-width (if vertical-image (w vertical-image) 0))
	     (border-height (if horizontal-image (h horizontal-image) 0))
	     (corner-width (if corner-image (w corner-image) 0))
	     (corner-height (if corner-image (h corner-image) 0))
	     (surface (sdl:create-surface (if vertical-image (+ width (* border-width 2)) width) (if horizontal-image (+ height (* border-height 2)) height))))
	(sdl:draw-surface background-texture :surface surface)

	;; Place the sides
	(when vertical-image
	  (let* ((banner (build-vertical-sides vertical-image (+ height (-  border-height (1+ corner-height))) color-key-pos))
		 (size (+ (* corner-height 2) (h banner)))
		 (size-difference (- (h surface) size))
		 (start-pos (round size-difference 2)))
	    (sdl:draw-surface-at-* banner  0  (+ corner-height start-pos)  :surface surface) ; left side
	    
	    (sdl:draw-surface-at-* banner (+ border-width width) (+ corner-height start-pos)  :surface surface)))

	(when horizontal-image
	  (let* ((banner (build-horizontal-sides horizontal-image (+ width (-  border-width (1+ corner-width))) color-key-pos))
		 (size (+ (* corner-height 2) (w banner)))
		 (size-difference (- (w surface) size))
		 (start-pos (round size-difference 2)))
	  (sdl:draw-surface-at-* banner (+ start-pos corner-width)  0 :surface surface) ; top
	  (sdl:draw-surface-at-* banner  (+ start-pos corner-width) (+ border-height height) :surface surface))) ; bottom
	    

	;; Place the corners
	(when corner-image
	  (sdl:draw-surface-at-* corner-image 0 0 :surface surface)
	  (sdl:draw-surface-at-* corner-image (+ width (- (* border-width 2) (w corner-image))) 0 :surface surface)
	  (sdl:draw-surface-at-* corner-image 0 (- (+ (* border-height 2) height) corner-height) :surface surface)
	  (sdl:draw-surface-at-* corner-image (+ width (- (* border-width 2) (w corner-image)))
				 (- (+ (* border-height 2) height) corner-height) :surface surface))
	(setf background (vector surface (vector (- (x menu-pos) border-width) (- (y menu-pos) border-height))))))))
