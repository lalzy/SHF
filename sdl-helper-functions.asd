;;;; sdl-helper-functions.asd


(asdf:defsystem #:sdl-helper-functions
  :description "Describe sdl-helper-functions here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:shf-error-handling ; remove commented file when done
	       #:alexandria
	       #:uiop
	       #:cl-utilities
               #:bt-semaphore
	       #:cl-unicode
	      ; #:cffi
               #:lispbuilder-sdl
               #:lispbuilder-sdl-mixer
               #:lispbuilder-sdl-ttf
               #:lispbuilder-sdl-image
               #:lispbuilder-sdl-gfx)
  :serial t
  :components ((:file "package")
	       (:file "Shape-classes")
	       (:file "sprites")
	       (:file "collision-checks")
	       (:file "states")
	       (:file "sound-system")
	       (:file "text-input")
               (:file "sdl-helper-functions")))
