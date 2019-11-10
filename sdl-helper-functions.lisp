;;;; sdl-helper-functions.lisp
;;;
;;;
;;;   About:
;;;   Code Conventions:
;;;    Aux:
;;;      Used to store objects, or macro gensyms
;;;
;;;
;;;
;;;   Go to Shape-classes.lisp, SHF-Error-Handling and change the respected functions for your implementation
;;;       > Also alternativly create a new create-exe function for your implementation
;;;   
;;;   NOTE! Do not use MP3(or use smpeg.dll),
;;;            causes crash on exit
;;;   
;;; WORKING ON:
;;;   Text read\write system, Text Areas 
;;;       > Rewrite the way line-wrap drawing works[Done]
;;;           Seperate it so there is only one function that takes a sequence of strings and draws those as lines
;;;           Then have a seperate function calculate this list with linewrap(using current linewrap algorythm)[Done]
;;;   
;;;          > Bonus:
;;;               Break loop when out of bounds
;;;               if it's a vector list, calculate where the next line would be read, then start loop\drawing from there
;;;                    Also take an optional parameter for length to reduce potentional calculations needed
;;;      
;;;      > Convert main2 test\prototype code for text field + scrollbar into an actual text area system\class[Done]
;;;         > Make the scrollbar\box\text scrollable(through engine, not game code)[Half Done]
;;;         > Make the scrollbar button\box sprite-able
;;;         > In support of sprite-able, ensure scroll\pixel movement is doable with static scroll button size.
;;;      
;;;      > Create a text-input test
;;;   
;;;   Do git stuff
;;;   
;;; TODO:
;;;   Rewrite the textfield\area scrolling system to not have a hitbox(on the scroll box\area),
;;;      and instead just use the box itself as mouse collision check(may have to create new method for collision check)
;;; 
;;;   Add a menu system
;;;     > Tied in with state system?
;;;     > Create a keybind system
;;;
;;;   Add Animation System
;;;     > Might have to rewrite draw-sprites to ensure main sprite from sprite-class is the one drawn
;;; 
;;;   Text Read and write system(text areas) - 320|28 - 780|345
;;;       > scrollbar
;;;
;;;   > Tidy source files
;;;
;;;   -- Maybe\lowpriority --
;;;
;;;   Multiple playlists for Music?
;;;
;;;   Ensure collision detection only happens on visible objects(key-ed parameter, visible)?
;;;     > Also sprite collision if no hitboxes are found in sprite
;;;
;;;   Incoperate a physics engine(Box2D, or ODE?)
;;;
;;;   Rewrite scroll-bar\box system to use clossures instead of global variable to reset the active box
;;;
;;;   Support setting a begining position for text in text-field   
;;;     > Requires rewrite of many functions to use that new start
;;;          position, instead of using 0
;;;
;;; ------ Performance concerns ------
;;;
;;;  The Linewrapping function\system:
;;;     > It constantly does the calculation for each word, while it only draws what can be seen, the amount of words
;;;        can potentionally cause high performance loss.
;;;        Solution to this is to change it to not handle drawing, but instead output a list or an array
;;;        That already represents the line\list system, to then draw those in sequence, rather than checking individual words
;;;
;;; ------------- Done ---------------
;;;
;;;   (done)Create a new project for error detection, this will ensure error messages pop up if libraries fail to load(done)
;;;       > Create a log-system that will create a log file, just in case
;;;
;;;   (Done)Cirlce Edge Collision
;;;
;;;   (done)Rework Sound\music to be one function\macro for the getting file\init file stuff.(done)
;;;
;;;   Create error Messages for:
;;;       > Sprite\image loading(Done)
;;;       (done)>  Try to create a dynamic \ function\macro for errors(Done)
;;;      
(in-package #:sdl-helper-functions) ; nicknamed shf for easier access

(defparameter *width* nil) ; Screen width
(defparameter *height* nil) ; Screen height

(defparameter *debug* nil) ; Debugging mode
(defparameter *debug-hitbox-draw* nil) ; Drawing the hitboxes
(defparameter *Current-mouse-button* nil)
(defparameter *mouse-state* 0)
(defparameter *mouse-move-direction* #(none none))
(defparameter *cursor* nil)
(defparameter **cursor-offset** nil)
;(defparameter *mouse-held-scroll-box* nil) ;; Used for scrolling
(defparameter *scroll-boxes-list* nil) ; List of scroll boxes for the use of emptying when not clicked

(defun CFFI-init ()
  (cffi:define-foreign-library sdl
      (:windows "sdl.dll"))
  (cffi:use-foreign-library sdl)
  
  (cffi:defcfun ("SDL_WarpMouse" warp-mouse-at-*) :void
    (x :unsigned-short)
    (y :unsigned-short)))

  ;; Create alternative mouse-setter
(defun warp-mouse (point)
  (when (or (not (vectorp point)) (> (length point) 2))
    (error "Only accepts a vector of 2 cordinate points(x|y)"))
  (warp-mouse-at-* (elt point 0) (elt point 1)))


(defun create-cursor (img-src &key (offset #(0 0)) (color-key #(0 0)))
  "Creates a custom mouse-cursor from an image"
  (setf *cursor* (sdl:blit-surface
		  (sdl:load-image "c:/te/cursor.bmp" :color-key-at color-key))
	*cursor-offset* offset)
  (sdl:show-cursor nil))

#||
(defun init (width height fps capture-mouse default-font)
  "Initialisation"
  
  (sdl:init-video)
  (sdl:enable-unicode)
  (if width
      (setf *width* width)
      (setf *width* (elt (sdl:video-dimensions) 0)))
  
  (if height
      (setf *height* height)
      (setf *height* (elt (sdl:video-dimensions) 1)))

  (when capture-mouse
    (sdl:sdl-wm-grab-input :sdl-grab-on))

    
       ;; Attempts to initialize the default font
       (shf-error:try-retry (let ((font (if default-font
					    default-font
					    (make-instance 'sdl:ttf-font-definition
							   :size *font-size*
							   :filename  (merge-pathnames (first *fonts*) *font-path*)))))
			      (sdl:initialise-default-font font))
			    :text (format nil "Cannot initialize the default font: ~a ~a ~a"  (elt *fonts* 0) " in " *font-path*))
       

       (sdl-mixer:open-audio :frequency *sound-frequency*)
       (CFFI-init)
       )

||#
(defun mouse-move-direction (old-x x old-y y)
  "Get the direction the mouse is moving"
  (let ((vertical (cond
		    ((= old-y y) 'none)
		    ((> old-y y) 'up)
		    (t 'down)))
	
	(horizontal (cond ((= old-x x) 'none)
			  ((> old-x x) 'left)
			  (t 'right))))
    
    (vector horizontal vertical)))


(defmacro with-window (width height fps title icon fullscreen borderless default-font capture-mouse init-form &body body
		       &aux (position (gensym))
			 (font (gensym)))
  `(sdl:with-init ()
     ;;(init ,width ,height ,fps ,capture-mouse ,default-font)

  
     (sdl:init-video)
     (sdl:enable-unicode)
     (if ,width
	 (setf *width* ,width)
	 (setf *width* (elt (sdl:video-dimensions) 0)))
     
     (if ,height
	 (setf *height* ,height)
	 (setf *height* (elt (sdl:video-dimensions) 1)))
     
     (when ,capture-mouse
       (sdl:sdl-wm-grab-input :sdl-grab-on))
     
     
     ;; Attempts to initialize the default font
     (shf-error:try-retry (let ((,font (if ,default-font
					  ,default-font
					  (make-instance 'sdl:ttf-font-definition
							 :size *font-size*
							 :filename  (merge-pathnames (first *fonts*) *font-path*)))))
			    (sdl:initialise-default-font ,font))
			  :text (format nil "Cannot initialize the default font: ~a ~a ~a"
					(elt *fonts* 0) " in " *font-path))
     
     
       (sdl-mixer:open-audio :frequency *sound-frequency*)
       (CFFI-init)
       

     
     ,init-form ; Our init form, a list of functions that'll run at this point
     
     
     (let ((,position (when ,borderless #(0 0))))
       (sdl:window *width* *height* :title-caption ,title :no-frame ,borderless
		   :fullscreen ,fullscreen :position ,position))
     
     (setf (sdl:frame-rate) ,fps)
     ,@body))



(defmacro main-loop (&key (width nil) (height nil) (fps 120) (title "test") (icon nil) (fullscreen nil) (borderless nil)
		       (draw-sprites t) (default-font nil) (capture-mouse nil)
		       init-form (quit-form t) main-form end-form pre-loop-form
		       mouse-button-down-form mouse-button-up-form mouse-motion-form
		       key-down-form key-up-form
		       ;; Local variable gensym
		     &aux (position (gensym)))
  
  "Keywords:
     fps - Target Frames Per Second
     Width\Height - The width\height of window(Do not use with borderless!)
     Title - The window Title
     Fullscreen - Fullscreen mode
     Borderless - borderless fullscreen(windowed without borders)

     init-form takes a function and will run it before the main loop.
     quit-form take a function and will run them at quit-event
     main-form take a function and will run it every frame of the main loop
     end-form take a function and will run at end of program even if it crashes"
  ;; Doesn't quit when an un-handleded error message occur as long as *debug* is on

  #||
      ;; Starts SDL and creates a window
      `(sdl:with-init ()
	 (init ,width ,height ,fps ,capture-mouse ,default-font)
	 ,init-form ; Our init form, a list of functions that'll run at this point

	 
	 (let ((,position (when ,borderless #(0 0))))
	   (sdl:window *width* *height* :title-caption ,title :no-frame ,borderless
		       :fullscreen ,fullscreen :position ,position))
	 
	 (setf (sdl:frame-rate) ,fps)
  ||#
  `(with-window ,width ,height ,fps ,title ,icon ,fullscreen ,borderless ,default-font ,capture-mouse ,init-form 
	       
	 ;; Event Loops
	 (unwind-protect
	      (let ((previous-x nil)
		    (previous-y nil)
		    (move-dir nil))
		(sdl:with-events ()
		  (:quit-event () ,quit-form t) ; Our quit form, same concept as init-form
		  (:key-down-event (:key key :unicode unicode)
				   ;; Adds key presses to global variables
				   (setf *key-pressed-code* (list key unicode))
				   (setf *key-pressed-state* (sdl:key-state-p))
				   ,key-down-form)
		  (:key-up-event (:key key :unicode unicode)
				 ;; Removes keypresses from global variables
				 (setf *key-pressed-code* (list nil unicode))
				 (setf *key-pressed-state* (sdl:key-state-p))
				  ,key-up-form
				 )
		  (:mouse-button-down-event (:BUTTON BUTTON :STATE STATE :X X :Y Y)
					    (setf *current-mouse-button* button
						  *mouse-state* state)
					    ,mouse-button-down-form)
		  
		  (:mouse-button-up-event (:BUTTON BUTTON :STATE STATE :X X :Y Y)
					  (setf *current-mouse-button* button
						*mouse-state* state)
					  (dolist (sb *scroll-boxes-list*)
					    (setf (is-active? sb) nil))
					  ,mouse-button-up-form)
		  
		  (:mouse-motion-event (:state state :x x :y y)
				       (setf *mouse-move-direction* (mouse-move-direction previous-x x previous-y y)))
				       ,mouse-motion-form
		  (:idle () ; Main-loop, clears and updates display

			 (sdl:clear-display (get-color black))
			 ,main-form ; Our main form, which consist of the actual game logic
			     
			 ;; Will draw sprites automatically(hitboxes if the global hitbox variable is set)
			 (when ,draw-sprites
			   (draw-sprites) 
			   (draw-hitboxes))


			 ;; Test later after changing to Norwegian computer
#||
			 (draw-text (format nil "random-test - ~a" (sdl:GET-GLYPH-METRIC #\a :metric :maxy))
				    #(0 50))

			 ||#
			 ;; Mouse direction
			 (setf previous-x (sdl:mouse-x)
			       previous-y (sdl:mouse-y))
			 
			 (setf *mouse-move-direction* #(none none))

			 ;; Draws custom cursor
			 (when *cursor*
			   (sdl:draw-surface-at *cursor* (vector (- (sdl:mouse-x) (elt *cursor-offset* 0))
								 (- (sdl:mouse-y) (elt *cursor-offset* 1)))))
			 ;; Update display
			 (sdl:update-display)))
		(sdl-ttf:quit-ttf)
		(sdl-mixer:halt-music)
		(sdl-mixer:close-audio t)
		(sdl:free sdl:*default-font*)
		,end-form))))




#+:ccl (defun create-exe (name function &key (exe-extention? t) (path "C:/te/") (app-type :gui) (error-hook t))
		"Create an executable"
		(shf-error:release-error error-hook)
  (ccl:save-application (merge-pathnames path (format nil "~a~:[~;.exe~]" name exe-extention?))
			:application-type app-type
			:toplevel-function function :prepend-kernel t))




;; Can't return it, for unkown reasons



;; All passable keywords
(defparameter *keywords* '(:main :end :init :fps :width :height))

(defun is-keyword (item)
  (member item *keywords*))

(defmacro key-sort (args 
		    &aux (main-form (gensym)) (end-form (gensym))
			   (init-form (gensym)) (tmp-fps (gensym)) (window-params (gensym)))
  "Sorts"
  `(let ((list '(,@args))
	 (,main-form)
	 (,init-form)
	 (,end-form)
	 (,tmp-fps 60)
	 (,window-params))
     (loop for item in list
        with current
	do


	  (case item 
	    (,*keywords* (setf current item)))

	  (unless (is-keyword item)
	    
	    (case current
	      (:main (push item ,main-form))
	      (:width (setf *width* item))
	      (:height (setf *height* item))
	      (:fps (setf ,tmp-fps item))
	      (:init (push item ,init-form))
	      (:end (push item ,end-form)))))
     
     (values ,tmp-fps (reverse ,init-form) (reverse ,main-form) (reverse ,end-form) )))


  #||
	    (cond ((string-equal current :main)
		   (push item ,main-part))
		  ((string-equal current :end)
		   (push item ,end-part))
		  ((string-equal current :init)
		   (push item ,init-part)))))
||#

(defun is-function? (symbol)
  "Checks if the passed symbol is a function"
  (if (symbolp symbol)
      (functionp (symbol-function symbol))))


(defun run-form (form-list)
  "Runs through the list in the form and evaluates it"
  (mapcar #'(lambda (x)
	      ;; Old non-eval code, doesn't work as we might have nested function calls inside
	      ;;     the function passed that doesn't get evaluated properly
	      #|(and (listp x) (is-function? (first x))
	           (apply (first x) (rest x)))|#

	      
	      ;; So instead, we evaluate the code here
	      (eval x))
	  form-list))

;; Make other keywords
;; ALternativly, (&key ) for keywords
(defmacro new-main ( &rest args &aux (font (gensym)) (main-form (gensym))
				  (end-form (gensym)) (init-form (gensym)) (tmp-fps (gensym)))
  "Main sdl loop"
  ;; Change to use window-param
  (setf *width* 800)
  (setf *height* 640)
  `(let (,main-form ,init-form ,end-form ,tmp-fps)
     (setf (values ,tmp-fps ,init-form ,main-form ,end-form) (key-sort ,args))
     
     
     (sdl:with-init ()
       (sdl:init-video)
       (sdl:enable-unicode)
       ;; Attempts to initialize the default font
       (let ((,font (make-instance 'sdl:ttf-font-definition
				 :size 15
				 :filename  "c:/te/vera.ttf")))
	(sdl:initialise-default-font ,font))
      (sdl:window *width* *height* :title-caption "new-main-test")

      (run-form ,init-form)

      
      (setf (sdl:frame-rate) ,tmp-fps)
      
      (sdl:with-events ()
		  (:quit-event () (run-form ,end-form) t) 

		  (:idle ()
			 (sdl:clear-display (get-color black))
			 (run-form ,main-form)
			 
			 ;; Update display
			 (sdl:update-display))))))
