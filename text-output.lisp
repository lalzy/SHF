(in-package #:sdl-helper-functions)

(defparameter *font-path* "")
(defparameter *fonts* '("vera.ttf"));'("vera.ttf")) ; list of font names
(defparameter *font-size* 15) ; Default font size

#| Rewrite how line-wrapping and line-drawing works, to support the new filter-out-colors list.
  Pass a string, then go through filter-out-colors to get an array, seperate this into a new array consisting of:
  #((words) color)


Current linewrap:
   list of strings representing individual lines.
   ("one line"
   "two line"
   "three line")

New list:
   A sequence of sequences representing lines, with a vector containing strings and the color to draw
   ((("some colored word in blue" blue) ("another color here" red))
    (("a new line, wih some color" green) ("and even more here" white))
    (("and then this is just a line of a unified color" white)))

|#

(defun change-default-font (font sdl:*default-font*)
  "changes default font to passed font"
  (unless (and (not (null font)) (sdl:initialise-default-font font))
    (error "Cannot initialize the default font.")))

(defun get-font (&key (font (first *fonts*)) (size 15))
  "Creates and initialise a font instance"
  (sdl:initialise-font (make-instance 'sdl:ttf-font-definition :size size :filename (merge-pathnames font *font-path*))))

(defun create-rgb-color (colors)
  (sdl:color :R (get-option-from-alist 'r colors 0)
	     :G (get-option-from-alist 'g colors 0) 
	     :B (get-option-from-alist 'b colors 0)))

(defmacro add-rgb-color (color-symbol command rgb-colorset-list)
  `(push (list ,color-symbol (parse-integer (subseq ,command 1))) ,rgb-colorset-list))


(defun sep-command (command)
  (cadr (uiop:split-string command :separator '(#\-))))

(defparameter *previous-font* nil)
(defun create-options (string)
  ;; Create a system that allows:
  ;; and a system for c-[color]|c-green  for pre-selected color
  (let ((commands (uiop:split-string string :separator '(#\,)))
	(rgb-colorset)
	(font-size)
	(font)
	(options))
    (dolist (command commands)
      (setf command (string-trim '(#\space) command))
      (case (aref command 0)
	(#\r (add-rgb-color 'r command rgb-colorset))
	(#\g (add-rgb-color 'g command rgb-colorset))
	(#\b (if (string= (aref command 1) #\g)
		 (sbreak "bg = ~a" command) ; BG system for text to be implemented!
		 (add-rgb-color 'b command rgb-colorset)))
	(#\c ; Make validity check
	 (push (list 'color (subseq command 2)) options)) ; make subseq to get sdl color
	
	(#\f (if (string= (aref command 1) #\z)
		 (setf font-size (parse-integer (sep-command command)))
		 (setf font (cadr (uiop:split-string command :separator '(#\")))
		       *previous-font* font)))
	(#\s ; make validity check
	 (push (list 'string-type (subseq command 2)) options))))

    ;; Create the colors
    (when rgb-colorset
      (push (list 'color (create-rgb-color rgb-colorset)) options))

    ;; Creates the font for current text
    (cond ((and font font-size)
	   (push (list 'font (get-font :font font :size font-size)) options))
	  (font
	   (push (list 'font (get-font :font font)) options))
	  (font-size
	   (if *previous-font*
	       (push (list 'font (get-font :font *previous-font* :size font-size)) options)
	       (push (list 'font (get-font :size font-size)) options))))
    
    options))

(defun filter-out-options (string)
  "#[R234G23B2]"
  (let (output)
    (loop for i to (last-index string)
       with start with end
       with new-string = (create-adj-string)
       with text-start = 0
       with text-end
       with option-list
       finally (push-last (list (subseq string text-start) option-list) output)
       do
	 (cond ((and (string= (aref string i) #\#) (string= (aref string (1+ i)) #\[))
		(setf text-end i)
		(setf start (incf i 2)) ; option for next text start here
		(let ((sub-text (subseq string text-start text-end)))
		  (when (> (length sub-text) 0)
		    (push-last (list sub-text option-list) output)))

		;; Get the sequence of options inbetween the '[]' pair
		(loop for j from start to (last-index string) do
		     (when (string= (aref string j) #\])
		       (setf end j
			     i j
			     text-start (1+ i))
		       (return)))
		(setf option-list (create-options (subseq string start end))))
	       (t (vector-push-extend (aref string i) new-string))))
    output))

(defun draw-text (point string &key args
				 (default-color (get-color white)) (font sdl:*default-font*)
				 (surface sdl:*default-surface*))
  "Colors are defined as RGB values after \#c, so \#c255000255 would be --"
  (iter (for s :in-sequence (filter-out-options (apply #'format nil `(,string ,@args))))
	(with string-to-draw)
	(with x = (aref point 0))
	(setf string-to-draw (elt s 0))

	(when (> (length string-to-draw) 0)
	  (draw-string string-to-draw x (aref point 1) (cadr s) default-color font surface))
	(incf x (w string-to-draw (get-option-from-alist 'font (cadr s) font)))))


(defun draw-string (string x y options default-color base-font surface)
  (let ((color (get-option-from-alist 'color options default-color))
	(font (get-option-from-alist 'font options base-font)))
    (sdl:draw-string-solid-* string x y :color color :font font :surface surface)))

#|
(case type
(:solid 
(sdl:draw-string-solid-* (string char) x y :color color :font font :surface surface))
(:blended (sdl:draw-string-blended-* (string char) x y :color color :font font :surface surface))
(:shaded (sdl:draw-string-shaded-* (string char) x y color bg-color :font font :surface surface))))

[lines][text][parameter]
( ;lines
( ; texts
("the text"
( the-options))
)

)
text = (
("Heyo here is a long list of stuff," NIL) 
(" and stuff, and stuff, you know, so yeah, it is quite long, as it should be" ((SDL-HELPER-FUNCTIONS::COLOR #<LISPBUILDER-SDL:COLOR #x2102E5D7AD>))))



(("heyo here" nil) ("is a list" red) ("of stuff" blue)) - 

((("heyo here" nil) ("is" red))
(("a list" red) ("of" blue))
(("stuff" blue)))
|#


(defun line-wrapping (text boundry &key (font sdl:*default-font*) (start-pos 0))
  (line-wrap-calc (filter-out-options text) boundry font start-pos))

#||
(defun line-wrap-calc (texts boundry font start-pos)
  "Seperate a list of words into lines of words based on maximum line-length allowed to be drawn(for use with draw-lines function)"
  (let ((x start-pos)
	(output nil)
	(current-line)
	(sentence ""))

  ;; loop through the texts and options
  (dolist (text texts)
    (let* ((options (cadr text)))

      ;; loop through the seperate words
      (dolist (word (uiop:split-string (car text) :separator '(#\space)))
	(setf sentence (format nil (if (> (length word) 0) "~a ~a" "~a~a") sentence word))
	(incf x (w word (get-option-from-alist 'font options font)))
	;(incf x (w word))

	;; When beyond boundry, add sentence to the line with option.
	(when (> x boundry)

	  ;; Make it move the out-of-bounds word to next line
	  (push-last (list sentence options) current-line)
	  (push-last current-line output)
	  (setf current-line nil 
		sentence ""
		x start-pos)))

      ;; Add the current sentence options to the current line in order to ensure
      ;; different options still count.
      (when (> (length sentence) 0)
	(push-last (list sentence options) current-line)
	(setf sentence ""))))

  ;; Ensures last line gets outputted
  (when current-line
    (push-last current-line output))
output))||#



;; Fix it so that it moves down the word when the word would be out of bounds(unless no word on current line*)
;; Also create support for \n and \r character.
(defun line-wrap-calc (texts boundry font start-pos)
  "Seperate a list of words into lines of words based on maximum line-length allowed to be drawn(for use with draw-lines function)"
  (let ((x start-pos)
	(output nil)
	(current-line)
	(sentence ""))

  ;; loop through the texts and options
  (dolist (text texts)
    (let* ((options (cadr text)))

      ;; loop through the seperate words
      (dolist (word (uiop:split-string (car text) :separator '(#\space)))
	(setf sentence (format nil (if (> (length word) 0) "~a ~a" "~a~a") sentence word))
	(incf x (w word (get-option-from-alist 'font options font)))
	;(incf x (w word))

	;; When beyond boundry, add sentence to the line with option.
	(when (> x boundry)
	  
	  ;; Make it move the out-of-bounds word to next line
	  (push-last (list sentence options) current-line)
	  (push-last current-line output)
	  (setf current-line nil 
		sentence ""
		x start-pos)))

      ;; Add the current sentence options to the current line in order to ensure
      ;; different options still count.
      (when (> (length sentence) 0)
	(push-last (list sentence options) current-line)
	(setf sentence ""))))

  ;; Ensures last line gets outputted
  (when current-line
    (push-last current-line output))
  output))

(defun draw-texts (point lines-to-draw &key args
				 (default-color (get-color white)) (font sdl:*default-font*)
				 (surface sdl:*default-surface*))
  "Colors are defined as RGB values after \#c, so \#c255000255 would be --"
  (let ((y (aref point 1))
	(font-height 0))

    ;; Loop through each individual line of the text
    (dolist (line lines-to-draw)
      (let ((x (aref point 0)))

	;; Loop through the individual texts to draw(with it's unique options)
	(dolist (text-list line)
	  (let* ((text (car text-list))
		(options (cadr text-list))
		(current-font-height (h text (get-option-from-alist 'font options font))))
	    (draw-string text x y options default-color font surface)
	    (setf font-height (if (> current-font-height font-height) current-font-height font-height))
	    ;(setf height (h text (get-option-from-alist 'font options font)))
	    (incf x (w text (get-option-from-alist 'font options font))))))
      (incf y font-height))))
