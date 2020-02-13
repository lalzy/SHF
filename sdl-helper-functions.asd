;;;; sdl-helper-functions.asd


(asdf:defsystem #:sdl-helper-functions
  :description "Describe sdl-helper-functions here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:shf-error-handling 
           #:shf-SDL-CFFI
	       #:iterate
	       #:cffi
	       #:trivial-dump-core
	       #:alexandria
	       #:closer-mop
	       #:uiop
	       #:cl-utilities
               #:bt-semaphore
	       #:cl-unicode
               #:lispbuilder-sdl
               #:lispbuilder-sdl-mixer
               #:lispbuilder-sdl-ttf
               #:lispbuilder-sdl-image
               #:lispbuilder-sdl-gfx)
  :serial t
  :components ((:file "package")
	       
	       ;; Core building blocks
	       (:file "utilities")
	       (:file "colors")
	       (:file "Shape-classes")

	       (:file "hitboxes")
	       
	       ;; Image\Sprites
	       (:file "images")
	       (:file "sprites")

	       ;; Collision\Physics
	       (:file "collision-checks")
	       
	       ;; Sound
	       (:file "sound-system")

	       ;; Input and text
	       (:file "text-output") ; Fonts, Text
	       (:file "scrollbars")
	       (:file "controll-input") ;K&M primarily
	       (:file "textfield")
	       (:file "text-input") ; Input into textfield

	       (:file "context-menu")
	       (:file "element-positioning")

	       
	       ;; Main
	       (:file "states")
               (:file "sdl-helper-functions")))

