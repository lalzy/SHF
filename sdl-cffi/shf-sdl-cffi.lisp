;;; Handles all SDL-CFFI code
(in-package #:shf-sdl-cffi)

(cffi:defcfun ("SDL_WarpMouse" warp-mouse-at-*) :void
  (x :unsigned-short)
  (y :unsigned-short))

;; Create alternative mouse-setter
(defun warp-mouse (point)
  (when (or (not (vectorp point)) (> (length point) 2))
    (error "Only accepts a vector of 2 cordinate points(x|y)"))
  (warp-mouse-at-* (elt point 0) (elt point 1)))


#||
(defun render-unicode-solid (text &key font color (encoding :utf-16))
  (with-foreign-string (string text :encoding encoding)
    (%ttf-render-unicode-solid% sdl-font text sdl-color)))
||#
#||

(defun draw-string-solid (string p1 &key
                                 (justify :left)
                                 (surface *default-surface*)
                                 (font *default-font*)
                                 (color *default-color*))
  "See [DRAW-STRING-SOLID-*](#draw-string-solid-*).
||#

(defun draw-string-unicode-solid (string p1 &key ((justify :left) (surface *default-surface*) (font sdl:*default-font*) (color *default-color*))))
