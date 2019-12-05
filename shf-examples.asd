;;;; sdl-helper-functions.asd

(asdf:defsystem #:shf-examples
  :description "Describe sdl-helper-functions here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:iterate
               #:lispbuilder-sdl
	       #:sdl-helper-functions)
  :serial t
  :components ((:module "examples"
                :components
				((:file "package")
                 (:file "tetris")))))

