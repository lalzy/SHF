;;;; sdl-helper-functions.asd

(asdf:defsystem #:shf-sdl-cffi
  :description "Describe sdl-helper-functions here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:alexandria #:cffi #:lispbuilder-sdl #:lispbuilder-sdl-ttf)
  :serial t
  :components ((:module "sdl-cffi"
                :components
				((:file "package")
                 (:file "shf-sdl-cffi")))))

