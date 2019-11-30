(in-package #:sdl-helper-functions)

(defparameter *colors* `((white ,(sdl:color :r 255 :g 255 :b 255))
			 (black ,(sdl:color :r 0 :g 0 :b 0))
			 (darkgray ,(sdl:color :r 50 :g 50 :b 50))
			 (gray ,(sdl:color :r 160 :g 160 :b  160))
			 (lightgray ,(sdl:color :r 211 :g 211 :b 211))
			 (green ,(sdl:color :r 0 :g 255 :b 0))
			 (red ,(sdl:color :r 255 :g 0 :b 0))
			 (blue ,(sdl:color :r 0 :g 0 :b 255))
			 (cyan ,(sdl:color :r 0 :g 255 :b 255))
			 (yellow ,(sdl:color :r 255 :g 255 :b 0))))

(defmacro add-color (color &key (r 0) (g 0) (b 0))
  "Add a color to the *colors* list"
  `(push (list ',color (sdl:color :r ,r :g ,g :b ,b)) ,*colors*))

(defun find-color (color)
  "helper function for get-color"
  (cadr (assoc color *colors* :test #'string=)))

(defmacro get-color (color)
  "Returns a chosen color from the list of SDL colors found in *colors*"
  `(find-color ',color))

(defun get-color-at-pixel (point &key surface)
  "get the colors of a selected pixel at the position"
  (sdl:color-* (apply #'sdl:read-pixel point
		      (when (typep surface 'sdl:surface) (list :surface surface)))))

(defun get-color-at-pixels (x y &key surface)
  (get-color-at-pixel (vector x y surface)))
