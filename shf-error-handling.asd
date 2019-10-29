;;;; sdl-helper-functions.asd

(asdf:defsystem #:shf-error-handling
  :description "Describe sdl-helper-functions here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:ftw)
  :serial t
  :components ((:module "shf-error-handling"
                :components
				((:file "package")
                 (:file "shf-error-handling")))))

