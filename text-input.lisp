;;;; sdl-helper-functions.lisp
;;;  Test area - x=320 y=28 - x=780 y=345
;;;
;;; - Create default collision detection on text-field?

(in-package #:sdl-helper-functions)

(defparameter *key-pressed-code* nil) ; Last pressed key
(defparameter *key-pressed-state* nil) ; A list of all pressed keys
(defparameter *font-path* "") ; Path to fonts
(defparameter *fonts* '("vera.tff")) ; list of font names
(defparameter *font-size* 10) ; Default font size

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

(defclass text-field (rect)
  ((surface :accessor get-surface
	    :initarg :surface)
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
   (amount-of-lines :accessor get-line-amount
		    :Documentation "The amount of lines the text-field hold"
		    :initarg :line-amount)
   (hitbox :accessor get-hitbox
	   :initarg :hitbox)))


(defclass scroll-box (rect)
  ((surface :accessor get-surface
	    :initarg :surface)
   (color :accessor get-box-color
	  :initarg :color)
   (direction :accessor get-box-dir
	      :initarg :dir
	      :documentation "what cordinate to scroll")
   (hitbox :accessor get-hitbox
	   :initarg :hitbox)
   (active :accessor is-active-box?
	   :initform nil
	   :documentation "if the mouse is currently engaged in this box")))


(defclass scroll-bar (rect)
  ((surface :accessor get-surface
	    :initarg :surface)
   (show :accessor show-scroll-bar?
	 :initarg :show)
   (scroll-box :accessor get-scroll-box :initarg :scroll-box)
   (hitbox :accessor get-hitbox :initarg :hitbox)
   (bar-color :accessor get-bar-color :initarg :bar-color)
   (box-color :accessor get-box-color :initarg :box-color)))

(defun get-scrollbox-hitbox (scroll-bar)
  (get-hitbox (get-scroll-box scroll-bar)))

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


(defun get-font (&key (font (first *fonts*)) (size 15))
  "Creates and initialise a font instance"
  (sdl:initialise-font (make-instance 'sdl:ttf-font-definition :size size :filename (merge-pathnames font *font-path*))))

(defun draw-text (string point &key (color (get-color white)) (font sdl:*default-font*))
  "Draw a text string on screen"
  (sdl:draw-string-solid string point :color color :font font))


(defgeneric draw-text-with-line-wrap (sentence cordinates
				      &key color boundry-x surface font start-x start-y)
  (:documentation "Draws either a text string, or a sequence of strings that wraps when it goes beyond the chosen bounds(boundry-x)"))

(defmethod draw-text-with-line-wrap ((sentence string) cordinates
				     &key  (color (get-color white)) (boundry-x nil) surface
				       (font sdl:*default-font*) start-x start-y)
  (line-wraping (cl-utilities:split-sequence #\space sentence) cordinates  color boundry-x surface font start-x start-y))

;; Change words to do subseq of sentence
(defmethod draw-text-with-line-wrap ((sentence sequence) cordinates
				     &key (color (get-color white)) (boundry-x nil) surface
				       (font sdl:*default-font*) start-x start-y)

  (line-wraping sentence cordinates color  boundry-x surface font start-x start-y))



;; Rewrite to return a list with lists that represents the lines to draw as text.
;;   Then handle drawing seperatly
(defun line-wraping (strings cordinate color boundry-x surface font start-x-pos start-y-pos
		     &aux (start-pos 0) (x-pos) (y-pos)
		       (space (sdl:get-font-size " " :size :w :font font))
		       (height (sdl:get-font-size " " :size :h :font font)))
"Returns a vector with first value being the line amount, and second value the height of individual lines"
  
  (setf boundry-x (cond (boundry-x boundry-x)
			(surface (sdl:width surface))
			(t *width*))
	start-pos (cond (start-x-pos start-x-pos)
			(surface 0)
			(t (elt cordinate 0)))
	x-pos start-x-pos
	y-pos (cond (start-y-pos start-y-pos)
		    (surface 0)
		    (t (elt cordinate 1))))
  
  (iter (for i from 0 to (1- (length strings)))
	(with line-amount = 1)
	(finally (return (vector line-amount height)))
	
	(let* ((word (elt strings i))
	       (word-size (sdl:get-font-size word :size :w :font font)))
	  
	  ;; Check if the new word's positon will exceed the boundry set, if it does move it down one cordinate
	  (when (and (>= (+ x-pos word-size) boundry-x) (> i 0))
	    (incf line-amount)
	    (setf x-pos start-pos
		  y-pos (+ y-pos height)))

	  ;; Only draws what can be seen
	  (when (and (>  y-pos  (- (sdl:y surface)  height)) (<  y-pos (sdl:height surface)))
	    (sdl:draw-string-solid word (vector x-pos y-pos) :color color :font font :surface surface))
	  
	  ;; Get the next position for the new word
	  (incf x-pos (+ word-size space)))))

(defun draw-text-with-lines (words surface &key (font sdl:*default-font*)
					     (color (get-color white))
					     (height (sdl:get-font-size " " :size :h :font font))
					     (x-pos 0) (y-pos 0)
					     (start-line 0)
					     (end-line (1- (length words))))
  
  (loop for i from start-line to end-line do
	(let* ((word (elt words i)))
	  ;; Only draws what can be seen
	  (when (and (> y-pos (- (sdl:y surface) height)) (< y-pos (sdl:height surface)))
	      (sdl:draw-string-solid word (vector x-pos y-pos) :surface surface :font font :color color)))

	;; Exit loop when we exceed what will be visible
	
	;; Draw next line one down
       (incf y-pos height)))

(defun list-to-string-list (list)
  "Converts a list of strings into a single string as list"
  (list (string-right-trim '(#\space) (format nil "~{~a ~}" list))))

(defgeneric line-wrapping (string boundry &key font space))

(defmethod line-wrapping ((string string) boundry &key (font sdl:*default-font*)
						    (space (sdl:get-font-size " " :size :w :font font))
						    (start-pos 0))
  (line-wrap-calc (cl-utilities:split-sequence #\space string) boundry font space start-pos))

(defmethod line-wrapping ((string sequence) boundry  &key (font sdl:*default-font*)
						       (space (sdl:get-font-size " " :size :w :font font))
						       (start-pos 0))
  (line-wrap-calc string boundry font space start-pos))

(defun line-wrap-calc (words boundry font space start-pos)
  "Seperate a list of words into lines of words based on maximum line-length allowed to be drawn(for use with draw-lines function)"
 ; (unless space (setf space (sdl:get-font-size " " :size :w :font font)))
  (let ((line-size start-pos)
	(line-amount 1)
	(first-loop t) ; Dont do boundry calculation on first loop
	(line-list nil)
	(current-line nil))

    (dolist (word words)
      (let ((word-size (sdl:get-font-size word :size :w :font font)))

	;; Check if our line exceeds the maximum line size
	(if (and (>= (+ word-size line-size) boundry) (not first-loop))
	 ; (format t "yes! word = ~a~%" word)
	    (progn
	      (setf line-list (append line-list (list-to-string-list current-line)))
	      (setf line-size start-pos
		    current-line nil)
	      (incf line-amount)))
	
	      (incf line-size (+ space word-size))
	(setf first-loop nil) ; Turns off first-loop checky
	(setf current-line (append current-line (list word)))))

    ;; Adds last line to list
    (setf line-list (append line-list (list-to-string-list current-line)))
  
    ;; Returns the list of strings as an array, also the amount of lines and the font height as values
    (values (make-array (length line-list) :initial-contents line-list)
	    line-amount
	    (sdl:get-font-size " " :size :h :font font))))







(defun draw-debug-text (string point &key (color (get-color white)))
  "Draws text that will only show up if debug is true"
  (when *debug*
    (sdl:draw-string-solid string point :color color)))


(defun is-keys (&rest keys)
  "Takes a list of keys and check if it's been pressed(through shf's global variable)"
  (find-if #'(lambda (key) (member key *key-pressed-state*)) keys))

(defun get-pressed-key (&aux (key *key-pressed-code*))
  "Get the current pressed key as character"
  (unless key (setf key 0))
   ; (unless (is-keys :sdl-key-up :sdl-key-down :sdl-key-left :sdl-key-right :sdl-key-lshift :sdl-key-rshift)
      (code-char key))

(defun check-key (char)
  "checks if passed char is the pressed key"
  (when *key-pressed-code*
    (if (equalp char (code-char *key-pressed-code*))
	(code-char *key-pressed-code*)
	nil)))


(defun change-default-font (font sdl:*default-font*)
  "changes default font to passed font"
  (unless (and (not (null font)) (sdl:initialise-default-font font))
    (error "Cannot initialize the default font.")))



;; Make it support image and transperancy
(defun create-text-field (&key (x 0) (y 0) (w *width*) (h *height*) state
			    (background (get-color white))
			    (font sdl:*default-font*)
			    text
			    (text-x 0)
			    (text-y 0)
			    (hitbox-color background)
			    (alpha 255)
			    line-amount)
  "Rewrite to create a text-field based on height\width parameters, and optional background, 
also create collision detection for mouse

Get the x,y,width,height, create a surface with width\height and draw it"

  (let ((surface (sdl:create-surface w h  :alpha alpha)))
    (when background
      (sdl:draw-box-* 0 0 w h :surface surface
		      :color background))
    (make-instance 'text-field :surface surface :x x :y y :w w :h h :state state
		   :background background :line-amount line-amount :font font :alpha alpha
		   :text text :text-x text-x :text-y text-y

		   ;; Unsure about hitbox for text-field, might not have one
		   :hitbox (create-hitbox 'rect :x x :y y :w w :h h :color hitbox-color)
		   )))

(defgeneric change-surface (object &key alpha))


(defmethod change-surface (object  &key alpha)
  (let* ((old-surface (get-surface object))
	 (surface (sdl:create-surface (sdl:width old-surface) (sdl:height old-surface))))
    (setf (get-surface object) surface)))


(defmethod change-surface ((object text-field) &key alpha)
  (let* ((old-surface (get-surface object))
	 (surface (sdl:create-surface (sdl:width old-surface) (sdl:height old-surface) :alpha alpha)))
    
    ;; Ensures the textbox box-field is drawn on the surface before anything else
    (when (text-field-background object)
      (sdl:draw-box-* 0 0 (w object) (h object) :surface surface
		      :color (text-field-background object)))
  (setf (get-surface object) surface)))

(defmethod change-surface ((object scroll-bar) &key alpha)
  (let* ((old-surface (get-surface object))
	 (surface (sdl:create-surface (sdl:width old-surface) (sdl:height old-surface) :alpha alpha)))

    ;; Surface for the scroll-box is the same as the scroll-bar
    (setf (get-surface (get-scroll-box object)) surface)
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
  (loop for i to (1- (length (get-text text-field)))
     with text = (get-text text-field)
     with longest-line = (elt text 0)
     with current-line
     finally (return longest-line)
     do
       (setf current-line (elt (get-text text-field) i))
       (when (> (length current-line) (length longest-line))
	 (setf longest-line current-line))))

(defun horizontal-text-size (text-field)
  "The pixel-size of the longest sentence of all the lines"
  (sdl:get-font-size (get-longest-line text-field) :size :w :font (get-text-font text-field)))

(defun max-horizontal-scroll-distance (text-field)
  "The maximum amount of characters that can be seen"
  (- (horizontal-text-size text-field)
     (w text-field)))

;; scrolling

(defun scrolly (text-field scroll-bar dir max)
  "Handles Horizontal Scrolling"
  (let ((slot (if (string-equal dir 'y) :h :w))
	(new-pos (scrolling-calc text-field scroll-bar dir max)))
    (if (> new-pos (end-of-elements text-field slot))
	(end-of-elements text-field slot)
	new-pos)))


(defun text-scrolling (text-field scroll-bar
		       &aux (scroll-box (get-scroll-box scroll-bar)))
  "Scrolls the text inside a text-field, with a scroll-bar"
  (when (scrolling scroll-bar)
    (when (and (string-equal (get-box-dir scroll-box) 'y)
	       (> (hidden-vertical-lines text-field) 0)) ; Ensure no scrolling if there are no lines to scroll
      ;(setf (get-text-y text-field ) (- (vertical-scroll text-field scroll-bar 'y))))
      
      (setf (get-text-y text-field ) (- (scrolly text-field scroll-bar 'y (max-vertical-scroll-distance text-field)))))
    
    (when (string-equal (get-box-dir scroll-box) 'x)
      (let ((max (max-horizontal-scroll-distance text-field)))
	(when (> max 0)
	  ;(setf (get-text-x text-field) (- (horizontal-scroll text-field scroll-bar max))))))))
	  (setf (get-text-x text-field) (- (scrolly text-field scroll-bar 'x max))))))))

(defun draw-text-on-text-field (textfield &key text (color (get-color white)))
  "draws lines of text ontop of a text field"
  (shf:draw-text-with-lines  (if text text (get-text textfield))
			     (get-surface textfield)
			     :x-pos (get-text-x textfield) :y-pos (get-text-y textfield)
			     :font (get-text-font textfield) :color color))

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

(defun create-scroll-bar (x y w h &key  (show t) (bar-color (get-color darkgray)) (alpha 255)
				    (sb-x 0) (sb-y 0) (sb-w w) (sb-h h) (direction 'y)
				    (sb-color (get-color lightgray)) (sb-hitbox-color sb-color))
  "Creates a scroll-bar"
  (let* ((surface (sdl:create-surface w h :alpha alpha)))
    (make-instance 'scroll-bar :surface surface :w w :h h :x x :y y :bar-color bar-color  :show show
		   :scroll-box (create-scroll-box surface x y sb-x sb-y sb-w sb-h direction sb-color sb-hitbox-color)))) ;:box-color box-color)))


(defun add-to-scroll-box-list (scroll-box-instance)
  (setf *scroll-boxes-list* (cons scroll-box-instance *scroll-boxes-list*))
  scroll-box-instance)

(defun create-scroll-box (surface bar-x bar-y box-x box-y box-w box-h direction color hb-color)
  "Creates the box used for scrolling in a scrollbar"
  (add-to-scroll-box-list (make-instance 'scroll-box :surface surface :w box-w :h box-h :x box-x :y box-y :color color :dir direction
					 :hitbox (create-hitbox 'rect :x bar-x :y bar-y :w box-w :h box-h  ;(1+ box-h)
								:color hb-color))))


(defun draw-bar (scroll-bar)
  (sdl:draw-box-* 0 0 (w scroll-bar) (h scroll-bar) :surface (get-surface scroll-bar) :color (get-bar-color scroll-bar)))
  ;(when (scroll-bar-show? scroll-bar)
  ;  (sdl:draw-surface-at-* (get-surface scroll-bar) (x scroll-bar) (y scroll-bar))))

(defun draw-scroll-bar (scroll-bar) ;scroll-box)
  "Draw the scroll-bar and scroll-box boxes to the scroll-bar surface, then draws the surface to screen"
    (draw-bar scroll-bar)
    (draw-scroll-box (get-scroll-box scroll-bar)) ;(get-scroll-box scroll-bar))
  ;(draw-scroll-box scroll-box)
					;(sdl:draw-box-* (x scroll-box) (y scroll-box) (w scroll-box) (h scroll-box))
  (when (show-scroll-bar? scroll-bar)
    (sdl:draw-surface-at-* (get-surface scroll-bar) (x scroll-bar) (y scroll-bar))))


;; Rewrite so size is better created, more similar to text editors
(defun calculate-scroll-box-height (scroll-bar text-height line-amount &key (min-size 5))
  ""
      (let* ((max (round (/ (h scroll-bar) text-height )))
	     (hidden-vertical-lines (- line-amount max))
	     (height (if (<= hidden-lines 0)
			 0
		    (round (/ (h scroll-bar)
			      
			      ;; Allows scrolling if only one line is hidden
			      (if (= hidden-lines 1) 2 hidden-lines))))))
	      ;; If there are no lines hidden from view, make the scroll-box an height of 0
	(values (if (< height min-size) min-size height)
		max)))


(defun draw-scroll-box (scroll-box)
  "Draws the scroll-box"

  (sdl:draw-box-* (x scroll-box)
		  (y scroll-box) (w scroll-box) (h scroll-box)
		  :surface (get-surface scroll-box)
		  :color (get-box-color scroll-box)))

(defun scroll-box-active-mouse? (scroll-box )
  "Changes the active state of the scroll-box if mouse is clicked on it"
  (and (shf:mouse-collision-check (get-hitbox scroll-box)) (sdl:mouse-left-p)
       (setf (is-active-box? scroll-box) t)))

  
(defun out-of-bounds? (box-pos box-size bar-size)
  "Checks if the scroll box is out of bound of the bar"
  (cond ((<= box-pos 0)
	 'start)
	((>= (+ box-size box-pos) bar-size)
	 'end)))

(defun get-new-scroll-box-pos (mouse-pos bar-pos bar-size box-pos  box-size)
  "Returns the new position of the scroll box and it's hitbox in relative to the mouse position"
  (let ((bounds (out-of-bounds? box-pos box-size bar-size)))
    (cond ((and (string-equal bounds 'start) (< mouse-pos (+ bar-pos (round (/ box-size 2)))))
	   (values 0 bar-pos))
	  ((and (string-equal bounds 'end) (> mouse-pos (- (+ bar-size bar-pos) (round (/ box-size 2)))))
	   (values (- bar-size box-size)
		   (+ bar-pos box-pos)))
	  (t (values (- (- mouse-pos bar-pos) (round (/ box-size 2)))
		     (- mouse-pos (round (/ box-size 2))))))))
#||
(defun set-scroll-box-pos (dir size scroll-bar mouse &aux (scroll-box (get-scroll-box scroll-bar))
					    (hitbox (get-hitbox scroll-box)))
  (setf (values (slot-value scroll-box dir) (slot-value hitbox dir))
	(get-new-scroll-box-pos mouse
				(slot-value scroll-bar dir) (slot-value scroll-bar size))))
||#

;; Rework scrolling to use relative position
(defun scrolling (scroll-bar &aux (scroll-box (get-scroll-box scroll-bar))
			       (hitbox (get-hitbox scroll-box)))
  "Call to automatically check for, and cause scrolling.
Destructivly change the positions of scroll-bar, scroll-box(and it's hitbox)
Returns nil if not scrolling"
  (scroll-box-active-mouse? scroll-box)

  (if (is-active-box? scroll-box)
      (cond ((string-equal (get-box-dir scroll-box) 'y)
	     (setf (values (y scroll-box) (y hitbox))
		   (get-new-scroll-box-pos (sdl:mouse-y) (y scroll-bar)  (h scroll-bar)
					   (y scroll-box) (h scroll-box))))
	    
	    ((string-equal (get-box-dir scroll-box) 'x)
	     (setf (values (x scroll-box) (x hitbox))
		   (get-new-scroll-box-pos (sdl:mouse-x) (y scroll-bar) (w scroll-bar)
					   (x scroll-box) (w scroll-box)))))
      nil))
