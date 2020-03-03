;;;; sdl-helper-functions.lisp
;; FIx assets path and such.

(in-package #:shf-test)


(defun main12 ()
  "Selection Square"
  (let (prev-x clicked prev-y)
    (shf:main-loop
     (:font-path "c:/te/" :width 500 :height 500)
     (:mouse-down (unless clicked (setf clicked t) (setf prev-x (sdl:mouse-x)
							 prev-y (sdl:mouse-y))))
     (:mouse-up (setf clicked nil))
     
     (:main
      (sdl:draw-box-* 0 0 500 500 :color (shf:get-color green))
      (when clicked
	(let ((a-surface (sdl:create-surface (-(sdl:mouse-x) prev-x) (- (sdl:mouse-y) prev-y) :alpha 90)))
	    (sdl:draw-box-* 0 0 (sdl:width a-surface) (sdl:height a-surface) :surface a-surface :color (shf:get-color blue))
	    (sdl:draw-surface-at-* a-surface prev-x prev-y)))))))

(defun main11 ()
  "Fog of war test"
  (let ((x 50) (y 50)  bg fog(text-timer 0) text)
    (shf:main-loop
     (:font-path "C:/te/" :width 1500 :height 500)
     (:init (setf bg (sdl:load-image "c:/te/images/map-fog-test.png"))
	    (setf fog (sdl:create-surface 500 500 :color-key (shf:get-color cyan))))
     (:main
      (sdl:draw-filled-circle-* x y 30  :color (shf:get-color cyan) :surface fog )
      (sdl:draw-surface bg)
      (sdl:draw-surface fog)
      
      (let ((darkening (sdl:create-surface 500 500 :color-key (shf:get-color cyan) :alpha 200)))
	(sdl:draw-filled-circle-* x y 30 :color (shf:get-color cyan) :surface darkening) ; Light circle

	;; Torch need to change color to determine brightness
	(sdl:draw-filled-circle-* x y 30 :color (shf:get-color orange)  :surface darkening)
	(sdl:draw-surface darkening))

	

      (sdl:draw-box-* (- x 2) (- y 2) 5 5 :color (shf:get-color yellow))

      (when (> text-timer 0)
	(shf:draw-text  #( 0 400) text)
	(decf text-timer 1))
      
      (cond ((shf:is-keys :sdl-key-down) (incf y))
	    ((shf:is-keys :sdl-key-up) (decf y)))
      
      (cond ((shf:is-keys :sdl-key-left) (decf x))
	    ((shf:is-keys :sdl-key-right) (incf x)))

      
      (cond  ((shf:is-keys :sdl-key-s)
	      (setf text-timer 60)
	      (setf text "saved!")
	      
	      (sdl:clear-color-key :surface fog) ; Remove the color keying for fog surface, this both disables and clear the actual color
	      (sdl:save-image fog "c:/te/sdl/fog.bmp") ; saves the image(no png added)
	      (sdl:enable-color-key t :surface fog) ; re-enables color keying
	      (setf (sdl:color-key fog) (shf:get-color cyan))) ; re-add cyan as the fog color

	     ;; Loading is simple, it just loads up the .bmp file and apply the color key immediatly
	     ((shf:is-keys :sdl-key-l)
	      (setf fog (sdl:load-image "c:/te/sdl/fog.bmp" :color-key (shf:get-color cyan)))
	      (setf text-timer 60
		    text "loaded!")))))))

(defun main8 ()
  "element-positioning-test"
  (let ()
    (shf:main-loop
     (:width 500 :height 500 :font-path "c:/te/")
     (:init)
     (:main))))

(defun main7 ()
  "Context-menu-test"
  (let ((clicked nil) (pressed nil))
    (shf:main-loop
     (:width 500 :height 500 :font-path "C:/te/")
     (:init (shf:initialize-context-menu)
	    (shf:set-browse-keys :SDL-KEY-DOWN :SDL-KEY-UP :SDL-KEY-RETURN :SDL-KEY-ESCAPE))
     (:key-down (when (shf:is-keys :SDL-KEY-SPACE)
		  (shf:create-context-menu '("one" "two" "three" "four" "five" "six" "seven" "height" "nine" "ten") :x 300 :y 200))
		(shf:with-state :context-menu
		  (setf pressed (shf:key-select-context-item))))
     (:mouse-down
      (shf:with-state :context-menu
	(setf pressed (shf::mouse-select-context-item)))
      (when (shf:is-mouse-key :right)
;	(shf:create-context-menu '("One" "Two" "three" "four" "five"))
	(shf:create-context-menu '("One" "Two" "three" "four" "five" "six" "7"))
	(shf:build-context-background (shf:make-image "background.png" :path "c:/te/images/")
				     :vertical-image (shf:make-image "side.png" :path "c:/te/images/") :color-key-pos #(0 0)
				     :horizontal-image (shf:make-image "side.png" :path "c:/te/images/")
				      :corner-image (shf:make-image "corners.png" :path "c:/te/images/")
				      ))
      #||
      (if (shf:check-state :context-menu)
	  (shf:clear-context-menu)
	  (shf::create-context-menu '("one" "two" "three") :spacing 5))||#
      (setf clicked t))
     (:main
      (shf:draw-text  #(0 0) (shf::strarg "current-state: ~a" shf::*STATE*))
      (shf:draw-text  #(0 20) (shf::strarg "last-clicked: ~a" pressed))
	(shf:draw-text  #(0 40) (shf::strarg "state = ~a" shf::*state*))
      (shf:with-state :context-menu
	(shf::run-context-menu)
	(shf:draw-text  #(0 60) (shf::strarg "mouse = ~a" shf:*mouse-move-direction*))
	(when clicked (setf clicked nil)))))))

(defun main6 ()
  "cursor test"
  (let ()
    (shf:main-loop
     (:font-path "c:/te/" :width 500 :height 500
		 :cursor "c:/te/cursor.png" :cursor-offset #(13 13))
     
    ;(:init (shf:create-cursor "C:/te/cursor.png" :offset #(13 13)))
     (:main (shf:draw-text  #(0 0) "yo")))))



(defun main2 ()
  "multi-line \ line-wrap test"
  (let ((text "Heyo here is a long #[b255]list of stuff,#[r255] and stuff, you know, so yeah, #[g255]it is#[r255] quite long, as it should be")
	(text2 "this is default font(vera),  #[f-\"ariblk.ttf\"]this is ariblk font, #[fz-25]Same font, but bigger size, #[f-\"vera.ttf\",fz-25]Vera font again, but bigger size as well"))
    (shf:main-loop
     (:font-path "c:/te/" :width 500 :height 500)
     (:init (setf text (shf:line-wrapping text 150))
	    (setf text2 (shf:line-wrapping text2 150)))
     (:key-down )
     (:main
      ;(let ((y 0))
	;(dolist (string text)
	 ;(shf::draw-texts (vector 0 0) text)))))
	 (shf::draw-texts (vector 0 0) text2)))))
	 ; (incf y (shf:h (car string)))))))))


;\\#c100255100!
(defun main1 ()
  "Multi-color-text-test"
    (shf:main-loop
     (:font-path "c:/te/" :assets-path "c:/te" :width 1500  :height 500)
     (:main
      (shf:draw-text  #(0 0) "welcome #[r255]to #[g152]many #[r123,g32,b12]different #[b255, g125, r5]colors#[b255]!#[g255]!#[r255]!")
      (shf:draw-text #(0 20) "this is default font(vera),  #[f-\"ariblk.ttf\"]this is ariblk font, #[fz-25]Same font, but bigger size, #[f-\"vera.ttf\",fz-25]Vera font again, but bigger size as well"))))

