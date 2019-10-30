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
   (state :accessor get-text-field-state
	  :initarg :state)
   (color :accessor get-box-color
	  :initarg :color)
   (alpha :accessor get-alpha
	  :initarg :alpha)
   (background :accessor text-field-background
	       :initarg :background)
   (foreground :accessor text-field-foreground
	       :initarg :foreground)
   (hitbox :accessor get-hitbox
	   :initarg :hitbox)))


(defclass scroll-box (rect)
  ((surface :accessor get-surface
	    :initarg :surface)
   (color :accessor get-box-color
	  :initarg :color)
   (hitbox :accessor get-hitbox
	   :initarg :hitbox)))


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
  `(push (list ',color (sdl:color :r r :g g :b b)) *colors*))

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
  
  (iter (for i from start-line to end-line)
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



(defun draw-text-on-text-field (textfield text x y &key (font sdl:*default-font*) (color (get-color white)))
  "draws lines of text ontop of a text field"
  (shf:draw-text-with-lines  text (get-surface textfield) :x-pos x :y-pos y :font font :color color))

(defun draw-text-field (textfield)
  "Draws the text field onto the screen"
  (sdl:draw-surface-at-* (get-surface textfield) (x textfield) (y textfield)))

(defun draw-text-field-with-text (textfield text &key (text-start-x 0) (text-start-y 0) (font sdl:*default-font*) (color (get-color white)))
  "Draws both the lines of text ontop of a text field, and the text field itself onto the screen"
  #||(sdl:draw-box-* 0 0 (w textfield) (h textfield) :surface (get-surface textfield)
		  :color (text-field-background textfield))|#
  
  (draw-text-on-text-field textfield text text-start-x text-start-y :font font :color color)
  (draw-text-field textfield))


(defun create-scroll-bar (x y w h &key  (show t) (bar-color (get-color darkgray)) (alpha 255)
				    (sb-x 0) (sb-y 0) (sb-w w) (sb-h h) (sb-color (get-color lightgray)) (sb-hitbox-color sb-color))
  "Creates a scroll-bar"
  (let* ((surface (sdl:create-surface w h :alpha alpha)))
    (make-instance 'scroll-bar :surface surface :w w :h h :x x :y y :bar-color bar-color  :show show
		   :scroll-box (create-scroll-box surface x y sb-x sb-y sb-w sb-h sb-color sb-hitbox-color)))) ;:box-color box-color)))

(defun create-scroll-box (surface bar-x bar-y box-x box-y box-w box-h color hb-color) ;&key (color (get-color lightgray)) (hitbox-color color))
  "Creates the box used for scrolling in a scrollbar"
    (make-instance 'scroll-box :surface surface :w box-w :h box-h :x box-x :y box-y :color color
		   :hitbox (create-hitbox 'rect :x bar-x :y bar-y :w box-w :h box-h ;(1+ box-h)
					  :color hb-color)))

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
	     (hidden-lines (- line-amount max))
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

;; Rework scrolling to use relative position
(defun scrolling (scroll-bar)
  "Call to automatically check for, and cause scrolling"
  (let* ((scroll-box (get-scroll-box scroll-bar))
	 (hitbox (get-hitbox scroll-box)))

    ;; Check if we click on the scroll-box
    (when (and (shf:mouse-collision-check hitbox)
	       (sdl:mouse-left-p))
      (setf *mouse-held-scroll-box* t))

    
    ;; Hitbox is the absolute screen cordinate, where as scroll-box is relative to the scroll-bar
    (when (and *mouse-held-scroll-box* )
      (setf (y scroll-box) (- (- (sdl:mouse-y) (y scroll-bar)) (round (/ (h scroll-box) 2)))
	    (y hitbox)  (- (sdl:mouse-y) (round (/ (h scroll-box) 2)))))

    ;; Stops the scroll-box when beyond the bounds of the scroll-bar
    (cond ((<= (y scroll-box) 0)
	   (setf (y scroll-box) 0
		 (y hitbox) (y scroll-bar)))
	  ((>= (+ (h scroll-box) (y scroll-box)) (h scroll-bar))
	   (setf (y scroll-box) (- (h scroll-bar) (h scroll-box))
		 (y hitbox) (+ (y scroll-bar) (y scroll-box)))))
    
    ;;(y scroll-box))) ;(- (sdl:mouse-y) 5)))
      ;(setf (y scroll-box) (- (sdl:mouse-y) (y scroll-box)))) ;(- (sdl:mouse-y) 5)))
    
      #||
    (cond ((<= (shf:y (shf:get-scroll-box scroll-bar)) #||*scroll-y*||# *box-y*) (setf (shf:y (shf:get-scroll-box scroll-bar)) #||*scroll-y*||# *box-y*))
	  ((>= (shf:y (shf:get-scroll-box scroll-bar)) #||*scroll-y*||# (+ *box-y* (- *tb-h* *scroll-box-size*)))
	   (setf (shf:y (shf:get-scroll-box scroll-bar)) ;;*scroll-y*
		 (+ *box-y* (- *tb-h* *scroll-box-size*)))))


    ;; Scroll text in textbox relative to position of scroll-bar's box
    (let* ((relative-box (- (shf:y (shf:get-scroll-box scroll-bar)) #||*scroll-y*||# *box-y*))
	   (max-box-pos  (- *tb-h* *scroll-box-size*))
	   (line-pixels (* (- *lines* *max-lines*) *height*))
	   (movement-rate (ceiling (/  line-pixels  (if (= max-box-pos 0) 1 max-box-pos)))))
      
      (setf *pos-y* (- (* movement-rate #||*box-y*||# relative-box ))));(- (* movement-rate relative-box))))
||#
    
    ))



;; Make it support image and transperancy
(defun create-text-field (&key (x 0) (y 0) (w *width*) (h *height*) state
			    (background (get-color white))
			    foreground
			    (hitbox-color background)
			    (alpha 255))
  "Rewrite to create a text-field based on height\width parameters, and optional background, 
also create collision detection for mouse

Get the x,y,width,height, create a surface with width\height and draw it"

  (let ((surface (sdl:create-surface w h :alpha alpha)))
    
    (sdl:draw-box-* 0 0 w h :surface surface
		    :color background)
					;:alpha alpha)
  #||(sdl:draw-box-* 0 0 (w textfield) (h textfield) :surface (get-surface textfield)
		  :color (text-field-background textfield) :alpha (get-alpha textfield))||#
    (make-instance 'text-field :surface surface :x x :y y :w w :h h :state state
		   :background background :foreground foreground :alpha alpha
		   :hitbox (create-hitbox 'rect :x x :y y :w w :h h :color hitbox-color))))

(defgeneric change-surface (object &key alpha))


(defmethod change-surface (object  &key alpha)
  (let* ((old-surface (get-surface object))
	 (surface (sdl:create-surface (sdl:width old-surface) (sdl:height old-surface))))
    (setf (get-surface object) surface)))


(defmethod change-surface ((object text-field) &key alpha)
  (let* ((old-surface (get-surface object))
	 (surface (sdl:create-surface (sdl:width old-surface) (sdl:height old-surface) :alpha alpha)))
    
    ;; Ensures the textbox box-field is drawn on the surface before anything else
    (sdl:draw-box-* 0 0 (w object) (h object) :surface surface
		    :color (text-field-background object))
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

