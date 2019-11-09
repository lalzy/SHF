;;;; sdl-helper-functions.lisp



(in-package #:shf-test)

(defun main4 ()
  (setf shf:*debug* t)
  (setf shf:*debug-hitbox-draw* t)
  
  (setf SHF:*font-path* "c:/te/")
  (setf SHF:*fonts* '("Vera.ttf"))
  
  (shf:main-loop
   :title "tester"
   :width 800
   :height 500
   :fps 60

   :quit-form (progn 
		(format t "quitting!~%"))
   :init-form 
   (progn
     (format t "initializing!~%"))

   :main-form
   (progn
     (shf:draw-text "In main!"  #(0 0))
     (shf:draw-text "Also in main!"  #(0 20)))))


(defun main3 ()
  (shf:new-main
   (format t "void!")
   :init (format t "initializing!~%")
   
   :main
     (shf:draw-text "In main!"  #(0 0))
     (shf:draw-text "Also in main!"  #(0 20))

   :end (format t "quitting!~%")))



(defun main2 ()
  (let ((scroll-points 0)
	(mouse-clicked nil)
	(scroll-stop nil)
	(init-string nil)
	(string "hello there this is a text string and stuff for the purpose of testing and stuff like that and such hello there this is a text string and stuff for the purpose of testing and stuff like that and such hello there this is a text string and stuff for the purpose of testing hello there this is a text string and stuff for the purpose of testing and stuff like that and such hello there this is a text string and stuff for the purpose of testing and stuff like that and such hello there this is a text string and stuff for the  this is the end")
	(text-field nil)
	(scroll-bar nil)
	(scroll-bar2 nil)
	(lines nil)
	(height nil))
    
    (setf shf:*debug* t)
    (setf shf:*debug-hitbox-draw* t)
    
    (setf SHF:*font-path* "c:/te/")
    (setf SHF:*fonts* '("Vera.ttf"))
    
    (shf:main-loop
     :title "tester"
     :width 800
     :height 500
     :fps 60
     :draw-sprites t
     ;:capture-mouse t
     ;:fullscreen t
     ;:borderless t
     ;; Appends the init-form:
     :quit-form (progn 
	(shf:empty-sprite-group))
     :init-form 
     (progn
       
       (let ((tf-w 150)
	     (tf-h 150)
	     (tf-x 200)
	     (tf-y 50)
	     (font (shf:get-font :size 15)))
	 (setf (values string lines height) (shf:line-wrapping string tf-w :font font))
	 (setf text-field (shf:create-text-field :x tf-x :y tf-y :w tf-w :h tf-h :font font)) ;:text string :line-amount lines))
	 (setf scroll-bar (shf:create-scroll-bar (+ tf-x tf-w) tf-y  5 tf-h :sb-h 31 :direction 'y :sb-hitbox-color (shf:get-color red) ))

	 
	 ;))
     (setf scroll-bar2 (shf:create-scroll-bar tf-x (+ tf-y tf-h) tf-w 5 :sb-w 30 :direction :x :sb-hitbox-color (shf:get-color red)))))

     :key-down-form
     (progn
       (shf:input-text-to-field text-field))
     
      ;; Appends the main form \ gameplay-loop
      :main-form
      (progn
	(shf:draw-text (format nil "~a , ~a" (sdl:mouse-x) (sdl:mouse-y)) #(0 0))

	(shf:draw-scroll-bar scroll-bar)
	(shf:draw-scroll-bar scroll-bar2)

	(setf (shf:is-active? text-field) (shf:mouse-collision-check text-field))
	;(setf (shf:is-active? text-field) )
#||
	(if (shf:mouse-collision text-field)
	    (setf (shf:is-active? text-field) t)
	    (setf (shf:is-active? text-field nil)))
||#
	(shf:text-scrolling text-field scroll-bar )
	(shf:text-scrolling text-field scroll-bar2 )

	(shf:draw-text-field-with-text text-field :color (shf:get-color blue))
	
       ;(shf:draw-hitboxes)

      ))))

(defun move-test (box)
  (let ((speed (cond ((shf:is-keys :sdl-key-rshift :sdl-key-lshift)
		      10)
		     ((shf:is-keys :sdl-key-lctrl :sdl-key-rctrl)
		      200)
		     (t 1))))
    (when  (shf:is-keys :sdl-key-left)
      (shf:move-sprite box :vertical (- speed)))
    (when (shf:is-keys :sdl-key-right)
      (shf:move-sprite box :vertical  speed))
    (when (shf:is-keys :sdl-key-up)
      (shf:move-sprite box :horizontal (- speed)))
    (when (shf:is-keys :sdl-key-down)
      (shf:move-sprite box :horizontal speed))))



 #||   
(defun state1 ()
  (sdl:draw-string-solid-* "We are in first state!" 0 20 :color (shf:get-color white)))

(defun state2 ()
  (sdl:draw-string-solid-* "We are in second state!" 0 20 :color (shf:get-color white)))
||#


(defun try-retry (try)
  (handler-case
       (funcall try)
     (error (e)
       (format t "Error!"))))

(defun main ()
  (let ((box nil)
	(box2 nil)
	(h1 nil)
	(h2 nil))
    
    (setf shf:*debug* t)
    (setf shf:*debug-hitbox-draw* t)
    
    (setf SHF:*font-path* "c:/te/")
    (setf SHF:*fonts* '("Vera.ttf"))
    
    (shf:main-loop
     :title "tester"
     :width 800
     :height 500
     :fps 15
     :draw-sprites nil
     ;:capture-mouse t
     ;:fullscreen t
     ;:borderless t
     ;; Appends the init-form:
     :init-form 
      (progn
      ; (shf:set-state "test")
       (shf:make-sprite-sheet "c:/te/images/sprites.png" '((0 0 32 32) (0 32 32 32) (500 500 32 32)) :color-key-pos #(0 0))
       
       (setf colide-once nil)
       
       ;(shf:init-sounds "c:/te/" :file-names '("hit.wav" "menu-move.wav"));'("hit.wav" "menu-move.wav"))
       ;(shf:init-sounds "c:/te/")
       ;(shf:init-sounds "c:/te/" :file-names '("hit" "menu-move"))
       (shf:init-sounds "c:/te/" :file-names '("menu-move"))
       (shf:init-sounds "c:/te/" :file-names '("hit"))
       
       ;(shf:init-music '("alone.mp3" "Brainless.mp3" "aevum.mp3" "alt.mp3"))
       ;(shf:init-music "c:/te/" :file-names '("track1" "track2" "track3" ))
       (shf:init-music "c:/te/" :file-names '("track1" ))
       (shf:init-music "c:/te/" :file-names '( "track2"))
       ;(shf:init-music "c:/te/" :file-names '( "track3" ))
       ;(shf:init-music "C:/te/")
       ;(shf:init-music "C:/te/" :extention ".mp3")
       
  
       (shf:empty-sprite-group)
       (setf box (shf:make-box-sprite 50 50 (shf:get-color darkgray) :x 140 :y 200))
       ;(setf box (shf:make-circle-sprite 50 (shf:get-color darkgrey) :x 140 :y 200))
       (setf box2 (shf:make-box-sprite 50 50 (shf:get-color darkgray) :x 230 :y 230))
       ;(setf box (shf:make-circle-sprite 50 (shf:get-color :green) :x 230 :y 230))

      
       
       (shf:create-hitbox 'rect :sprite box :x 15 :y 25 :w 10 :h 10 :name "boxy" :color (shf:get-color red))
       (shf:create-hitbox 'circle :sprite box :y 8 :r 5 :name "circly" :color (shf:get-color red))
       (shf:create-hitbox 'rect :sprite box2 :x 15 :y 15 :w 5 :h 5 :name "boxer"  :color (shf:get-color red))

      (setf h1 (shf:create-hitbox 'rect :w 200 :h 5 :x 50 :y 400 :name "h1 box" :color (shf:get-color blue)))
       (setf h2 (shf:create-hitbox 'circle :r 50 :x 350 :y 400 :name "h1 circle" :color (shf:get-color blue)))
      ; (shf:create-hitbox :circle :sprite box  :r 15 :color (shf:get-color :red))
       ;(shf:create-hitbox :circle :sprite box2  :r 15 :color (shf:get-color :red))
     ;  (shf:create-hitbox :rect :x 150 :y 0 :w 50 :h 50)

 ;      (setf (shf:get-sprite-hitboxes box)
;	     (list (shf:create-hitbox box :rect :x 10 :y 10 :w 30 :h 30 :color (shf:get-color :red))))
      )
      
      ;; Appends the main form \ gameplay-loop
      :main-form
      (progn

	
	(when (shf:is-keys :sdl-key-q)
	  (sdl:push-quit-event))
	
       (shf:draw-sprites)
       (shf:draw-hitboxes)
       
       (shf:draw-text (format nil "hello! - size = ~a" (sdl:get-font-size "hello!" :size :w)) #(0 0) )

      (shf:draw-text (format nil "current song = ~a" (shf:get-current-song-name)) #(410 0))

      (when (> delay 0)
       (when (shf:is-keys :sdl-key-z)  (shf:play-sound :hit) )
       (cond ((and (shf:is-keys :sdl-key-lshift) (shf:is-keys :sdl-key-1))
	      (shf:set-state "test"))
	     ((and (shf:is-keys :sdl-key-lshift) (shf:is-keys :sdl-key-2))
	      (shf:set-state "tester")))
       (when (shf:is-keys :sdl-key-x) (shf:play-sound :menu-move))
       (when (shf:is-keys :sdl-key-c) (shf:random-music) (shf:play-current-song))
       (when (shf:is-keys :sdl-key-v) (shf:stop-music))
       (when (shf:check-key #\+);(shf:is-keys :sdl-key-kp-plus)
	 (shf:set-volume 1) (shf:set-music-volume 1))
       (when (shf:check-key #\-);(shf:is-keys :sdl-key-kp-minus)
	 (shf:set-volume -1) (shf:set-music-volume -1))
       (when (shf:is-keys :sdl-key-g) (shf:play-song :track3))
       )

      (when (shf:is-keys :sdl-key-j)  (shf:delete-from-sprite-group box))
      (when (shf:is-keys :sdl-key-k) (shf:add-to-sprite-group box))
       (shf:music-stopped-form
	;(shf:rotate-playlist)
	;(shf:play-current-song)

	)

       (if (shf:is-keys :sdl-key-x :sdl-key-c :sdl-key-v :sdl-key-z :sdl-key-+ :sdl-key--)
	   (decf delay)
	   (setf delay 1))
      
       ;; If Q or escape is pressed, goes to quit-event
       (when (or (shf:is-keys :sdl-key-q) (shf:is-keys :sdl-key-escape))
	 (setf shf:*key-pressed-state* nil)
	 (sdl:push-quit-event))
       (when (shf:is-keys :sdl-key-s))


	(when (shf:edge-collision-check (first (shf:get-sprite-hitboxes box)) nil)
	   (format t "jup!~%"))
       
     ; (shf:show-menu-text '("start" "options" "quit"))
#||
       (let* ((text (shf:make-text-surface "testy test" :x 30 :y 150))
	     (x (elt (shf:get-text-point text) 0))
	     (y (elt (shf:get-text-point text) 1))
	     (w (elt (shf:get-text-point text) 2))
	     (h (elt (shf:get-text-point text) 3)))
	 (sdl:draw-surface-at (shf:get-text-surface text) (shf:get-text-point text))

       (when (and (and (> (sdl:mouse-x) x)
		      (> (sdl:mouse-y) y))
		 (and (< (sdl:mouse-x) (+ w x))
		      (< (sdl:mouse-y) (+ h y))))
	 (shf:draw-text (format nil "w = ~a h = ~a" (elt (shf:get-text-point text) 2)
				(elt (shf:get-text-point text) 3)) #(0 220))
	 (shf:draw-text "coliding!" #(0 260))))
       
	 (shf:draw-text (format nil "X = ~a Y = ~a" (sdl:mouse-x) (sdl:mouse-y)) #(0 240))
	 (shf:draw-text (format nil "width = ~a height = ~a" shf:*width* shf:*height*) #(0 300))
       ||#

	 (let ((collision (shf:collision-check box h1)))
	   (when collision
	     (shf:draw-text "You colided with the hitbox!" #(0 60))))
	 
	 (let ((collision (shf:collision-check box h2)))
	   (when collision
	     (shf:draw-text "You colided with the circle hitbox!" #(0 60))))
	 
	 ; chech collision with static box and moving box
	 (let ((collision  (shf:collision-check box box2))
	       (font (shf:get-font :size 25)))
	   (when collision
	     (when (shf:get-collision-hitbox-name "circly" collision)
	       (shf:draw-text (format nil "circly colliding!") #(0 40) :font font))
	     (when (shf:get-collision-hitbox-name "boxy" collision)
	      (shf:draw-text (format nil "boxy colliding!") #(0 60) :font font))
	     (when (shf:get-collision-hitbox-name "boxer" collision)
	       (shf:draw-text (format nil "boxer colliding!") #(0 80) :font font))))

	 (let ((collision (shf:mouse-collision-check box))
	       (font (shf:get-font :size 25)))
	   (when collision
	     (when (shf:get-collision-hitbox-name "circly" collision)
	       (shf:draw-text (format nil "mouse colided with circly!") #(0 340) :font font))
	     (when (shf:get-collision-hitbox-name "boxy" collision)
	       (shf:draw-text (format nil "mouse colided with boxy") #(0 340) :font font))))
	 (let ((collision (shf:mouse-collision-check box2))
	       (font (shf:get-font :size 25)))
	   (when collision
	     (when (shf:get-collision-hitbox-name "boxer" collision)
	       (shf:draw-text (format nil "mouse colided with boxer!") #(0 340) :font font))))

;; Collision range test

	 
	 (let* ((old-box (shf:clone-sprite box)))
	    ; ((old-box (vector (shf:x box) (shf:y box))))
	   (move-test box)

	  ; (format t "ob-hx-~a, b-hx-~a~%" (shf:x (second (shf:get-sprite-hitboxes old-box)))
	;	   (shf:x (first (shf:get-sprite-hitboxes box))))
	   
	   ;(format t "old x = ~a new x = ~a ~%" (shf:x old-box) (shf:x box))
	; (shf:draw-text (format nil "old-box pos = ~a ~a" (shf:x old-box) (shf:y box)) #(280 30))
	; (shf:draw-text (format nil "box pos = ~a ~a" (shf:x box) (shf:y box)) #(280 0))
	   ;; Change collision-range to:
	   ;;
	    (shf:collision-check box box2  old-box)
	   ;;
	   (when (shf:collision-check box box2  old-box)
	     (setf colide-once t)))
	 
	 (when  colide-once
	   (shf:draw-text "Colided!!!!" #(460 0) :color (shf:get-color red)))

	 (when (shf:get-edge-dir box 'left :beyond t)
	   (shf:set-sprite-pos box :x (+ (shf:x box) (shf:w box) shf:*width*)))
	 
	 (when (shf:get-edge-dir box 'right :beyond t)
	   (shf:set-sprite-pos box :x (- 0  (shf:w box))))
	 
	 (when (shf:get-edge-dir box 'top :beyond t)
	   (shf:set-sprite-pos box :y (+ (shf:y box) (shf:h box) shf:*height*)))
	 
	 (when (shf:get-edge-dir box 'bottom :beyond t)
	   (shf:set-sprite-pos box :y (- 0  (shf:h box))))

	 
	 (shf:draw-text (format nil "mouse pos = ~a, ~a" (sdl:mouse-x) (sdl:mouse-y)) #(150 0))
	 
	 (when (shf:mouse-collision-check h1)
	   (shf:draw-text "Mouse Collision! with rectangle!" #(0 340) :font (shf:get-font :size 18) :color (shf:get-color cyan)))

	 
      (when (shf:mouse-collision-check h2)
	(shf:draw-text "Mouse Collision! with circle!" #(0 340) :font (shf:get-font :size 18) :color (shf:get-color cyan)))
      
      (let* ((text "This text is mouse collision-based!")
	     (x 0)
	     (y 320)
	     (collision (shf:mouse-text-collision text x y :name "text-string-col" )))
	(shf:draw-text text (vector x y))
	(when collision
	  (shf:draw-text (format nil "text-collision! - name = [~a]" collision) #(0 340)
			 :font (shf:get-font :size 18) :color (shf:get-color cyan))))

      (let* ((text "this text is too")
	     (x 0)
	     (y 270)
	     (collision (shf:mouse-text-collision text x y :font (shf:get-font :size 18) :name "text-string-col-2" )))
	(shf:draw-text text (vector x y) :font (shf:get-font :size 18))
	(when collision
	  (shf:draw-text (format nil "text-collision! - name [~a]" collision) #(0 340)
			 :font (shf:get-font :size 18) :color (shf:get-color cyan))))


      
      (if (shf:check-key #\s)
	  (shf:draw-text (format nil "s code!") #(400 0))
	  (shf:draw-text (format nil "unicode=~a" (SHF:get-pressed-key)) #(400 0)))










      
;	(shf:create-cursor "c:/te/cursor.bmp")
      ))))

  (defun delay ())


(defparameter colide-once nil)
(defparameter delay 1)

