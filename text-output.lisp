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

(defun get-font (&key (font (first *fonts*)) (size 15))
  "Creates and initialise a font instance"
  (sdl:initialise-font (make-instance 'sdl:ttf-font-definition :size size :filename (merge-pathnames font *font-path*))))


(defun draw-string (char x y color font type &optional surface bg-color)
  (case type
    (:solid 
     (sdl:draw-string-solid-* (string char) x y :color color :font font :surface surface))
    (:blended (sdl:draw-string-blended-* (string char) x y :color color :font font :surface surface))
    (:shaded (sdl:draw-string-shaded-* (string char) x y color bg-color :font font :surface surface))))


(defun filter-color (string)
  ""
  (sdl:color :r (parse-integer string :start 0 :end 3)
	     :g (parse-integer (subseq string 3 6))
	     :b (parse-integer (subseq string 6 9))))

(defun new-empty-string ()
  (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t))

(defun filter-out-colors (string &optional default-color)
  ""
  (let ((return-sequence (make-array 1 :adjustable t :fill-pointer 0))
	(current-string (new-empty-string))
	(color default-color))
    (iter (for i to (last-index string))
	  (finally (vector-push-extend
		    (vector current-string (if color color default-color))
				       return-sequence))
	  (when (and (string= (aref string i) #\\) (string= (aref string (1+ i)) #\#))
	    (if (equalp (aref string (incf i 2)) #\c)
		(cond ( (numberp (parse-integer (string (aref string (1+ i))) :junk-allowed t))
		       (unless (= (length current-string) 0)
			 (vector-push-extend
			  (vector current-string color )
					     return-sequence)
			 (setf current-string (new-empty-string)))
			(setf color (filter-color (subseq string (1+ i) (incf i 10)))))
		      (t (incf i)
			 (unless (= (length current-string) 0)
			   (vector-push-extend
			    (vector current-string color )
					       return-sequence)
			   (setf current-string (new-empty-string)))
		       (setf color default-color)))
		(decf i 2)))

	  ;; fix linewrap as well
	  ;; Index out of bounds, fix with return of a vector (nil [color])
	  (cond ((>= (last-index string) i)
		 (vector-push-extend (aref string i) current-string))
		((= 0 (length current-string))
		 (setf current-string ""))))
    return-sequence))

(defun draw-text (string point &key (type :solid) (bg-color (shf:get-color yellow))
				 (default-color (get-color white)) (font sdl:*default-font*)
				 (surface sdl:*default-surface*))
  "Colors are defined as RGB values after \#c, so \#c255000255 would be --"
  (iter (for s :in-sequence (filter-out-colors string default-color))
	(with font-size)
	(with string-to-draw)
	(with x = (aref point 0))
	(setf string-to-draw (aref s 0))
	
	(setf font-size (sdl:get-font-size (aref s 0) :size :w :font font))
	(when (> (length string-to-draw) 0)
	  (draw-string string-to-draw x (aref point 1) (aref s 1) font type surface))
	(incf x font-size))

  ;;(draw-string string (aref point 0) (aref point 1) default-color font type surface bg-color))

)

(defun draw-text-with-lines (strings surface &key (font sdl:*default-font*)
					     (color (get-color white))
					     (height (sdl:get-font-size " " :size :h :font font))
					     (x-pos 0) (y-pos 0)
					     (start-line 0)
					     (end-line (1- (length strings))))
  (iter (for i from start-line to end-line)
	(let* ((line (aref strings i)))
	  ;; Only draws what can be seen
	  (when (and (> y-pos (- (sdl:y surface) height)) (< y-pos (sdl:height surface)) (> (length line) 0))
	    (draw-text line (vector x-pos y-pos) :surface surface :font font :default-color color)))
	    ;(sdl:draw-string-solid line (vector x-pos y-pos) :surface surface :font font :color color)))

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
  (line-wrap-calc (cl-utilities:split-sequence #\space string)
		  boundry font space start-pos))

(defmethod line-wrapping ((string sequence) boundry  &key (font sdl:*default-font*)
						       (space (sdl:get-font-size " " :size :w :font font))
						       (start-pos 0))
  
  (line-wrap-calc string boundry font space start-pos))

;; Rewrite to use array, reason for not using array is the lack of support in text-input-lisp and text-field-has-text?
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
    (values 
     (make-array (length line-list) :initial-contents line-list)
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


