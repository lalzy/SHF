(in-package #:sdl-helper-functions)

(defparameter *font-path* "") ; Path to fonts
(defparameter *fonts* '("vera.tff")) ; list of font names
(defparameter *font-size* 15) ; Default font size



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
	  (when (and (> y-pos (- (sdl:y surface) height)) (< y-pos (sdl:height surface)) (> (length word) 0))
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


(defun change-default-font (font sdl:*default-font*)
  "changes default font to passed font"
  (unless (and (not (null font)) (sdl:initialise-default-font font))
    (error "Cannot initialize the default font.")))



;; Make it support image and transperancy
(defun create-text-field (&key (x 0) (y 0) (w *width*) (h *height*) state
			    (background (get-color white))
			    (font sdl:*default-font*)
			    (text nil)
			    (text-x 0)
			    (text-y 0)
			    (active nil)
			    (hitbox-color background)
			    (alpha 255)
			    (line-amount 0))
  "Rewrite to create a text-field based on height\width parameters, and optional background, 
also create collision detection for mouse

Get the x,y,width,height, create a surface with width\height and draw it"

  (let ((surface (sdl:create-surface w h  :alpha alpha)))
    (when background
      (sdl:draw-box-* 0 0 w h :surface surface
		      :color background))
    (make-instance 'text-field :surface surface :x x :y y :w w :h h :state state
		   :background background :line-amount line-amount :font font :alpha alpha :active active
		   :text text :text-x text-x :text-y text-y

		   ;; Unsure about hitbox for text-field, might not have one
		   :hitbox (create-hitbox 'rect :x x :y y :w w :h h :color hitbox-color)
		   )))
