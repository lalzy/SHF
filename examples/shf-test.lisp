;;;; sdl-helper-functions.lisp



(in-package #:shf-test)

(defun main10 ()
  "random test"
  (let ()
    (shf:main-loop
     (:font-path "c:/te/" :assets-path "c:/te" :width 500 :height 500)
     (:main nil))))

(defun main6 ()
  "cursor test"
  (let ()
    (shf:main-loop
     (:font-path "c:/te/" :width 500 :height 500
		 :cursor "c:/te/cursor.png" :cursor-offset #(13 13))
     
    ;(:init (shf:create-cursor "C:/te/cursor.png" :offset #(13 13)))
     (:main (shf:draw-text "yo" #(0 0))))))

(defun main5 ()
  "animation testing"
  (let ()
    (shf:main-loop
     (:width 150 :height 150 :font-path "c:/te/")
     (:main (shf:draw-text "heyo" #(0 0))))))

(defun main4 ()
  "Image testing"
  (let (img img2 guy r g b points)
    (shf:main-loop
     (:width 500 :height 500 :title "image testing" :font-path "C:/quicklisp/local-projects/sdl-helper-functions/assets/")
     (:init 
      (setf img (shf:make-image "test.png" :path "C:/quicklisp/local-projects/sdl-helper-functions/assets/"))
	    (setf img2 (shf:make-image "test2.png" :path "C:/quicklisp/local-projects/sdl-helper-functions/assets/"))
	    (setf guy (shf:create-sprite "sprite.png" :path #p"c:/te/" :x 0 :y 0 :cells (shf:generate-sheet-cells 32 32))))
     (:main
      ;;(when (and (numberp (sdl:mouse-x)) (numberp (sdl:mouse-y)))
   
      (sdl:draw-surface-at-* img 150 0)
	    (sdl:draw-surface-at-* img2 150 260)
	    (sdl:draw-surface-at-* (shf:get-sprite guy) 0 0)

	    (shf:draw-text (format nil "r-~a, g-~a, b-~a" r g b) #(0 0))

	    (loop for point in points do
		 (sdl:draw-pixel point :color (shf:get-color white)))
	    (when (sdl:mouse-left-p)
	      (push (vector (sdl:mouse-x) (sdl:mouse-y)) points))
	    (setf (values r g b) (shf:get-color-at-pixel (vector (sdl:mouse-x) (sdl:mouse-y))))))))

(defun main3 ()
  "state testing"
  (let (box (box-dir #(:right :down)) (speed 5))
    (shf:main-loop
     (:width 400 :height 500 :title "some new title!" :font-path "c:/te/" ;:clear-color (shf:get-color black)
	     )
     (:quit )
     
     (:init (shf:add-state :pause)
	    (setf box (shf:make-circle-sprite 8 (shf:get-color green))))

     (:mouse-down (shf:with-state :game (cond ((shf:is-mouse-key :wheel-down)
			 (decf speed))
			((shf:is-mouse-key :wheel-up)
			 (incf speed)))))
     
     (:key-down
      (when (shf:is-keys :sdl-key-p :sdl-key-pause)
	(cond 
	  ((shf:check-state :pause) 
	   (shf:set-state :game))
	  ((shf:check-state :game)
	   (shf:set-state :pause))))
      
      (shf:with-state :game (when (shf:is-keys :sdl-key-q) (shf:set-state :quit)))
      (shf:with-state :menu (shf:set-state :game)))
     
     (:main
      (shf:with-state :menu
	(shf:draw-text "hello from main! Press anykey!" #(0 0))
		      ;; Vector of 3 items, non activated, activated(collision of mouse \ keyactive), state to change to
		      (let ((menu-items `((,(shf:make-image "newgame.png" :path "C:/te/assets/")
					    ,(shf:make-image "newgame2.png" :path "C:/te/assets/")
					    :game)
					  (,(shf:make-image "options.png" :path "c:/te/assets/")
					    ,(shf:make-image "options2.png" :path "c:/te/assets/")
					    :options)
					  (,(shf:make-image "quit.png" :path "c:/te/assets/")
					    ,(shf:make-image "quit2.png" :path "c:/te/assets/")
					    :quit))))
			(shf:create-menu menu-items #(40 40) :spacing 30)))
	
		      
      (shf:with-state :options
	(shf:set-state :menu))
      (shf:with-state :game
	;; collision
	(cond ((shf:get-edge-dir box 'right) (setf (elt box-dir 0) :left)
	       (shf:set-sprite-pos box :x(- shf:*width* (shf:w box))))
	      ((shf:get-edge-dir box 'left) (setf (elt box-dir 0) :right)
	       (shf:set-sprite-pos box :x 0))
	      
	      ((shf:get-edge-dir box 'bottom) (setf (elt box-dir 1) :up)
	       (shf:set-sprite-pos box :y (- shf:*height* (shf:h box))))
	      ((shf:get-edge-dir box 'top) (setf (elt box-dir 1) :down)
	       (shf:set-sprite-pos box :y 0)))
	
	;; movement
	(shf:move-sprite box :horizontal (if (string= (elt box-dir 0) :right) speed (- speed)))
	(shf:move-sprite box :vertical (if (string= (elt box-dir 1) :down) speed (- speed)))))

     (:post-draw
      (unless (shf:check-state :menu)
	(shf:draw-text (format nil "speed = ~a" speed) #(200 0)))
      (shf:with-state :pause
	(shf:draw-text "Game paused!" #(150 75) :color (shf:get-color red)))))))


(defun main2 ()
  "Textfield testing"
  (let ((scroll-points 0)
	(mouse-clicked nil)
	(scroll-stop nil)
	(init-string nil)
	(string "hello there this is a text string and stuff for the purpose of testing and stuff like that and such hello there this is a text string and stuff for the purpose of testing and stuff like that and such hello there this is a text string and stuff for the purpose of testing hello there this is a text string and stuff for the purpose of testing and stuff like that and such hello there this is a text string and stuff for the purpose of testing and stuff like that and such hello there this is a text string and stuff for the  this is the end")
	(text-field nil)
	(scroll-bar nil)
	(scroll-bar2 nil)
	(lines nil)
	(input nil)
	(height nil))
    
    (setf shf:*debug* t)
    (setf shf:*debug-hitbox-draw* t)
    (setf SHF:*fonts* '("Vera.ttf"))
    
    (shf:main-loop ;(shf:main-loop
     (:title "tester"
     :width 800
     :height 500
     :font-path "c:/te/"
     :fps 60)
     ;; Appends the init-form:
     (:quit(shf:empty-sprite-group))
     (:init (let ((tf-w 150)
		  (tf-h 150)
		  (tf-x 200)
		  (tf-y 50)
		  (font (shf:get-font :size 15)))
	      (setf (values string lines height) (shf:line-wrapping string tf-w :font font))
	      (setf text-field (shf:create-text-field :x tf-x :y tf-y :w tf-w :h tf-h :font font)) ;:text string :line-amount lines))
	      (setf scroll-bar (shf:create-scroll-bar (+ tf-x tf-w) tf-y  5 tf-h :sb-h 31 :direction 'y :sb-hitbox-color (shf:get-color red) ))
	      (setf scroll-bar2 (shf:create-scroll-bar tf-x (+ tf-y tf-h) tf-w 5 :sb-w 30 :direction :x :sb-hitbox-color (shf:get-color red)))))

     (:mouse-up (when (shf:mouse-collision-check text-field)
		  (setf (shf:is-active? text-field) t)))
     (:key-down
       (shf:input-text-to-field text-field :multi-lines t :count-lines nil :max-length 5))
     
      ;; Appends the main form \ gameplay-loop
      (:main
	(shf:draw-text (format nil "~a , ~a" (sdl:mouse-x) (sdl:mouse-y)) #(0 0))
	(shf:draw-text (format nil "text-field is ~:[not active~;active~]" (shf:is-active? text-field)) #(0 20))

	(shf:draw-scroll-bar scroll-bar)
	(shf:draw-scroll-bar scroll-bar2)

	(shf:draw-text (format nil "input: ~:[{}~; {~a}~]" input  input) #(250 0))

	(shf:draw-text (format nil "~a" (shf:get-pressed-key)) #(250 20))
	
	(when (shf:is-all-keys :sdl-key-return :sdl-key-lctrl)
			       
	  (setf input (first (shf:get-text text-field)))
	  (setf (shf:is-active? text-field) nil))
	
	
	;(setf (shf:is-active? text-field) (shf:mouse-collision-check text-field))
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
      (shf:move-sprite box :horizontal (- speed)))
    (when (shf:is-keys :sdl-key-right)
      (shf:move-sprite box :horizontal  speed))
    (when (shf:is-keys :sdl-key-up)
      (shf:move-sprite box :vertical (- speed)))
    (when (shf:is-keys :sdl-key-down)
      (shf:move-sprite box :vertical speed))))


(defun main ()
  "collision and sprite class testing"
  (let ((box nil)
	(box2 nil)
	(h1 nil)
	(h2 nil))
    
    (setf shf:*debug* t)
    (setf shf:*debug-hitbox-draw* t)
    
    (shf:main-loop
     (:title "tester"
	     :width 800
	     :height 500
	     :fps 60
	     
	     :cursor "c:/te/cursor.png"
	     :cursor-offset #(16 16)
     
	     :font-path "c:/te/"
	     :draw-sprites nil)
     ;; Appends the init-form:
     (:init 
      ; (shf:set-state "test")
      
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
      (:main (when (shf:is-keys :sdl-key-q)
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
	     (shf:music-stopped-form	;(shf:rotate-playlist)
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
	   (shf:draw-text "Colided!!!!" #(460 30) :color (shf:get-color red)))

	 
      (when (shf:is-keys :sdl-key-O) (setf colide-once nil))
	 
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

