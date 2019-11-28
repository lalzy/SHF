(in-package #:sdl-helper-functions)

(defclass text-field (rect)
  ((surface :accessor get-surface
	    :initarg :surface)
   (active :accessor is-active?
	   :initarg :active)
   (text :accessor get-text
	      :initarg :text
	      :documentation "text to be used on the text-field")
   (text-x :accessor get-text-x
	   :initarg :text-x)
   (text-y :accessor get-text-y
	   :initarg :text-y)
   (state :accessor get-text-field-state
	  :initarg :state)
   (color :accessor get-box-color
	  :initarg :color)
   (alpha :accessor get-alpha
	  :initarg :alpha)
   (font :accessor get-text-font
	 :initarg :font
	 :Documentation "Font used by the textfield")
   (background :accessor text-field-background
	       :initarg :background)
   (hitbox :accessor get-hitbox
	   :initarg :hitbox)))


;; Decleared here rather than textfield.lisp becaus of dependencies
(defun create-text-field (&key (x 0) (y 0) (w *width*) (h *height*) state
			    (background (get-color white))
			    (font sdl:*default-font*)
			    (text nil)
			    (text-x 0)
			    (text-y 0)
			    (active nil)
			    (hitbox-color background)
			    (alpha 255))
  "Rewrite to create a text-field based on height\width parameters, and optional background, 
also create collision detection for mouse

Get the x,y,width,height, create a surface with width\height and draw it"

  (let ((surface (sdl:create-surface w h  :alpha alpha)))
    (when background
      (sdl:draw-box-* 0 0 w h :surface surface
		      :color background))
    (make-instance 'text-field :surface surface :x x :y y :w w :h h :state state
		   :background background  :font font :alpha alpha :active active
		   :text text :text-x text-x :text-y text-y

		   ;; Unsure about hitbox for text-field, might not have one
		   :hitbox (create-hitbox 'rect :x x :y y :w w :h h :color hitbox-color)
		   )))


(defun get-line-amount (textfield)
  (length (get-text textfield)))

(defmethod change-surface ((object text-field) &key alpha)
  (let* ((old-surface (get-surface object))
	 (surface (sdl:create-surface (sdl:width old-surface) (sdl:height old-surface) :alpha alpha)))
    
    ;; Ensures the textbox box-field is drawn on the surface before anything else
    (when (text-field-background object)
      (sdl:draw-box-* 0 0 (w object) (h object) :surface surface
		      :color (text-field-background object)))
  (setf (get-surface object) surface)))

(defun change-text-field-state (text-field)
  (if (text-field-active? text-field)
      (setf (text-field-active?) nil)
      (setf (text-field-active? text-field) t)))

;; Text-field scrolling

(defun get-max-box-pos (scroll-bar slot)
  "Gets the scroll-boxes max pixel movement"
  (- (slot-value scroll-bar slot) (slot-value (get-scroll-box scroll-bar) slot)))

(defun get-pixel-movement-rate (text-field scroll-bar dir max &aux (scroll-box (get-scroll-box scroll-bar)))
  "Get how many pixels to scroll by"
  (let* ((slot (if (string-equal dir 'x) 'w 'h))
	 (max-box-pos (get-max-box-pos scroll-bar slot)))
    (ceiling (/ max (if (= max-box-pos 0) 1 max-box-pos)))))


(defun end-of-elements (text-field size)
  "Get where the last element pixel is"
  (if (string-equal size :h)
      (* (sdl:get-font-size " " :size size :font (get-text-font text-field)) (hidden-vertical-lines text-field))
      (max-horizontal-scroll-distance text-field)))

;; Rewrite this to allow for both pixels, and lines
(defun scrolling-calc (text-field scroll-bar dir max)
  "Calculates how much to scroll by"
  (* (get-pixel-movement-rate text-field scroll-bar dir max) (slot-value (get-scroll-box scroll-bar) dir)))

;; Vertical Scrolling

(defun text-field-shown-lines (text-field)
  "How many lines can be seen at any one time"
  (floor (/ (h text-field) (sdl:get-font-size " " :size :h :font (get-text-font text-field)))))

(defun hidden-vertical-lines (text-field)
  "Lines that we can't see in the text-field"
  (- (get-line-amount text-field) (text-field-shown-lines text-field)))

(defun max-vertical-scroll-distance (text-field)
  "The maximum amount that can be  scrolled vertically"
  (* (hidden-vertical-lines text-field)
     (sdl:get-font-size " " :size :h :font (get-text-font text-field))))


;; Horizontal Scrolling

(defun get-longest-line (text-field)
  "Loop through the text in the textfield, to get the sentence that's the longest"
  (when (> (length (get-text text-field)) 0)
    ;(loop for i to (last-index (get-text text-field))
    (iter (for i to (last-index (get-text text-field)))
       (with text = (get-text text-field))
       (with longest-line = (aref text 0))
       (with current-line)
       (finally (return longest-line))
       
       (setf current-line (aref (get-text text-field) i))
       (when (> (length current-line) (length longest-line))
	 (setf longest-line current-line)))))

(defun horizontal-text-size (text-field)
  "The pixel-size of the longest sentence of all the lines"
  (sdl:get-font-size (get-longest-line text-field) :size :w :font (get-text-font text-field)))

(defun max-horizontal-scroll-distance (text-field)
  "The maximum amount of characters that can be seen"
  (- (horizontal-text-size text-field)
     (w text-field)))

;; Generic scrolling

(defun scrolling (text-field scroll-bar dir max)
  "Handles Horizontal Scrolling"
  (let ((slot (if (string-equal dir 'y) :h :w))
	(new-pos (scrolling-calc text-field scroll-bar dir max)))
    (if (> new-pos (end-of-elements text-field slot))
	(end-of-elements text-field slot)
	new-pos)))


(defun text-scrolling (text-field scroll-bar
		       &aux (scroll-box (get-scroll-box scroll-bar)))
  "Scrolls the text inside a text-field, with a scroll-bar"
  (when (and (scroll-box scroll-bar) (get-text text-field))
    (when (and (string-equal (get-box-dir scroll-box) 'y)
	       (> (hidden-vertical-lines text-field) 0)) ; Ensure no scrolling if there are no lines to scroll
      (setf (get-text-y text-field ) (- (scrolling text-field scroll-bar 'y (max-vertical-scroll-distance text-field)))))
    
    (when (string-equal (get-box-dir scroll-box) 'x)
      (let ((max (max-horizontal-scroll-distance text-field)))
	(when (> max 0) ;; Ensure no scrolling if there are no characters to scroll
	  (setf (get-text-x text-field) (- (scrolling text-field scroll-bar 'x max))))))))

(defun text-field-has-text? (text)
  "Ensures there is text to be drawn"
  (and text (> (length text) 0)))



;;; Drawing

(defun draw-text-on-text-field (textfield &key text (color (get-color white)))
  "draws lines of text ontop of a text field"
  (when (or (text-field-has-text? (get-text textfield)) text)
    (shf:draw-text-with-lines  (if text text
				   (get-text textfield))
			       (get-surface textfield)
			       :x-pos (get-text-x textfield) :y-pos (get-text-y textfield)
			       :font (get-text-font textfield) :color color)))

(defun new-textfield-surface (textfield)
  ;; Creates a new surface for text-field(To reset text\colors etc)
  (setf (get-surface textfield) (sdl:create-surface (w textfield) (h textfield) :alpha (get-alpha textfield)))
  (when (text-field-background textfield)
    (sdl:draw-box-* 0 0 (w textfield) (h textfield) :surface (get-surface textfield)
		    :color (text-field-background textfield))))

(defun draw-text-field-with-text (textfield &key text (color (get-color white)))
  "Draws both the lines of text ontop of a text field, and the text field itself onto the screen"    
  ;; Ensures the textbox box-field is drawn on the surface before anything else
  (new-textfield-surface textfield)
  (draw-text-on-text-field textfield :text text :color color)
  (sdl:draw-surface-at-* (get-surface textfield) (x textfield) (y textfield)))

