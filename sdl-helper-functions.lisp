;;;; sdl-helper-functions.lisp
;;;
;;;
;;;   About:
;;;   Code Conventions:
;;;    Aux:
;;;      Used to store objects, or parts of global lists
;;;
;;;   Go to Shape-classes.lisp, SHF-Error-Handling and change the respected functions for your implementation
;;;       > Also alternativly create a new create-exe function for your implementation
;;;   
;;;   NOTE! Do not use MP3(or use smpeg.dll),
;;;            causes crash on exit
;;;   
;;; WORKING ON:
;;;   Use unified library for CCL specified things.
;;;   Optional character limit in textfield
;;;   
;;;   
;;;   
;;;   
;;; TODO:
;;;   Rewrite the textfield\area scrolling system to not have a hitbox(on the scroll box\area),
;;;      and instead just use the box itself as mouse collision check(may have to create new method for collision check)
;;; 
;;;  Go back through code and make use of:
;;;      Set-x\y  and incf-x\y
;;; 
;;;  Fix clone-sprite to use non-ccl class-slot and slot-definition-name
;;; 
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

(in-package #:sdl-helper-functions) ; nicknamed shf for easier access

(defparameter *width* nil) ; Screen width
(defparameter *height* nil) ; Screen height

(defparameter *debug* nil) ; Debugging mode
(defparameter *debug-hitbox-draw* nil) ; Drawing the hitboxes
					;(defparameter *mouse-held-scroll-box* nil) ;; Used for scrolling

(defmacro with-window (width height fps title icon fullscreen borderless position default-font font-path capture-mouse &body body)
  (alexandria:with-gensyms (font)
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

       (when ,font-path
	 (setf *font-path* ,font-path))
       
       ;; Attempts to initialize the default font
       (shf-error:try-retry (let ((,font (if ,default-font
					     ,default-font
					     (make-instance 'sdl:ttf-font-definition
							    :size *font-size*
							    :filename  (merge-pathnames ,font-path (first *fonts*))))))
			      (sdl:initialise-default-font ,font))
			    :text (format nil "Cannot initialize the default font: ~a ~a ~a"
					  (elt *fonts* 0) " in " ,font-path))
       
       
       (sdl-mixer:open-audio :frequency *sound-frequency*)
       (CFFI-init)
       
       (sdl:window *width* *height* :title-caption ,title :no-frame ,borderless
		   :fullscreen ,fullscreen :position ,position)
       
       (setf (sdl:frame-rate) ,fps)
       ,@body)))



(defmacro main-loop ((&key width height (title "working title") (fps 60) icon fullscreen borderless position
			   debug-mode capture-mouse (draw-sprites t) default-font font-path)
		     &rest body)
  
  (let (pre-window-form post-window-form main-form end-form key-down-form key-up-form
			mouse-down-form mouse-up-form window-focus-form mouse-move-form close-form)
    (loop for item in body do
	 (case (first item)
	   ((:pre-window-init :pre-init) (setf pre-window-form (rest item)))
	   ((:post-window-init :post-init :init) (setf post-window-form (rest item)))
	   ((:main :idle) (setf main-form (rest item)))
	   ((:close :finish :protect) (setf close-form (rest item))) 
	   ((:key-down :key-down-event) (setf key-down-form (rest item)))
	   ((:key-up :key-up-event) (setf key-up-form (rest item)))
	   ((:mouse-move :mouse-motion-event :mouse-motion) (setf mouse-move-form (rest item)))
	   ((:mouse-down :mouse-down-event :mouse-button-down-event) (setf mouse-down-form (rest item)))
	   ((:mouse-up :mouse-up-event :mouse-button-up-event) (setf mouse-up-form (rest item)))
	   ((:window-focus :winow) (setf window-focus-form (rest item)))
	   ((:quit :end :quit-event)  (setf end-form (rest item)))))

    
    (when borderless
      (unless position
	(setf position #(0 0))))

    (alexandria:with-gensyms (font previous-mouse-x previous-mouse-y mouse-move-direction)
      `(progn
	 ,@pre-window-form
	 
	 (with-window ,width ,height ,fps ,title ,icon ,fullscreen ,borderless ,position ,default-font ,font-path
	     ,capture-mouse
	   ,@post-window-form
	   (unwind-protect
		(let (,previous-mouse-x ,previous-mouse-y ,mouse-move-direction)
		  (sdl:with-events ()
		    (:quit-event () ,@end-form (setf *state* (first (last *states*))) (empty-sprite-group) t)

		    (:mouse-motion-event (:state state :x x :y y)
					 (setf *mouse-move-direction* (mouse-move-direction previous-x x previous-y y))
					 ,@mouse-move-form)
		    
		    (:mouse-button-down-event (:BUTTON BUTTON :STATE STATE :X X :Y Y)
					      (setf *current-mouse-button* button
						    *mouse-state* state)
					      ,@mouse-down-form)
		    
		    (:mouse-button-up-event (:BUTTON BUTTON :STATE STATE :X X :Y Y)
					    (setf *current-mouse-button* button
						  *mouse-state* state)
					    (dolist (sb *scroll-boxes-list*)
					      (setf (is-active? sb) nil))
					    ,@mouse-up-form)
		    
		    (:key-down-event (:key key :unicode unicode)
				     ;; Adds key presses to global variables
				     (setf *key-pressed-code* (list key unicode))
				     (setf *key-pressed-state* (sdl:key-state-p))
				     ,@key-down-form)
		    (:key-up-event (:key key :unicode unicode)
				   ;; Removes keypresses from global variables
				   (setf *key-pressed-code* (list nil unicode))
				   (setf *key-pressed-state* (sdl:key-state-p))
				   ,@key-up-form)

		    (:SYS-WM-EVENT ()
				   ,@window-focus-form)
		    
		    (:idle ()
			   (sdl:clear-display (get-color black))
			   ,@main-form
			   
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
			   
			   (sdl:update-display))
		    
		    (sdl-ttf:quit-ttf)
		    (sdl-mixer:halt-music)
		    (sdl-mixer:close-audio t)
		    (sdl:free sdl:*default-font*)
		    ,@close-form))))))))

#+:ccl (defun create-exe (name function &key (exe-extention? t) (path "C:/te/") (app-type :gui) (error-hook t))
	 "Create an executable"
	 (shf-error:release-error error-hook)
	 (ccl:save-application (merge-pathnames path (format nil "~a~:[~;.exe~]" name exe-extention?))
			       :application-type app-type
			       :toplevel-function function :prepend-kernel t))


#||
(WITH-EVENTS (TYPE)
 (:ACTIVE-EVENT (:GAIN GAIN :STATE STATE)
    ... )
 (:KEY-DOWN-EVENT (:STATE STATE :SCANCODE SCANCODE :KEY KEY :MOD MOD :UNICODE UNICODE)
    ... )
 (:KEY-UP-EVENT (:STATE STATE :SCANCODE SCANCODE :KEY KEY :MOD MOD :UNICODE UNICODE)
    ...)
 (:MOUSE-MOTION-EVENT (:STATE STATE :X X :Y Y :X-REL X-REL :Y-REL Y-REL)
    ...)
 (:MOUSE-BUTTON-DOWN-EVENT (:BUTTON BUTTON :STATE STATE :X X :Y Y)
    ...)
 (:MOUSE-BUTTON-UP-EVENT (:BUTTON BUTTON :STATE STATE :X X :Y Y)
    ...)
 (:JOY-AXIS-MOTION-EVENT (:WHICH WHICH :AXIS AXIS :VALUE VALUE)
    ...)
 (:JOY-BUTTON-DOWN-EVENT (:WHICH WHICH :BUTTON BUTTON :STATE STATE)
    ...)
 (:JOY-BUTTON-UP-EVENT (:WHICH WHICH :BUTTON BUTTON :STATE STATE)
    ...)
 (:JOY-HAT-MOTION-EVENT (:WHICH WHICH :HAT HAT :VALUE VALUE)
    ...)
 (:JOY-BALL-MOTION-EVENT (:WHICH WHICH :BALL BALL :X-REL X-REL :Y-REL Y-REL)
    ...)
 (:VIDEO-RESIZE-EVENT (:W W :H H)
    ...)
 (:VIDEO-EXPOSE-EVENT ()
    ...)
 (:USER-EVENT (:TYPE TYPE :CODE CODE :DATA1 DATA1 :DATA2 DATA2)
    ...)
 ||#
	 
