(in-package :sdl-helper-functions)

(defparameter *key-pressed-code* nil) ; List of current key pressed(first element is sdl key, second is unicode code)
(defparameter *key-pressed-state* nil) ; A list of the current key state


(defparameter *mouse-move-direction* #(none none)) ;direction mouse is currently moving

(defparameter *cursor* nil) ; Custom mouse cursor
(defparameter **cursor-offset** nil)
(defparameter *Current-mouse-button* nil)
(defparameter *mouse-state* 0)


(defun is-keys (&rest keys)
  "Takes a list of keys and check if it's been pressed(through shf's global variable)"
  (find-if #'(lambda (key) (member key *key-pressed-state*)) keys))

(defun get-pressed-key (&aux (key *key-pressed-code*))
  "Get the current pressed key as character"
  (when key
    (values (first key) (code-char (second key)))))

(defun check-key (char)
  "checks if passed char is the pressed key"
  (when *key-pressed-code*
    (if (equalp char (code-char (elt *key-pressed-code* 1)))
	(code-char (elt *key-pressed-code* 1))
	nil)))

(defun create-cursor (img-src &key (offset #(0 0)) (color-key #(0 0)))
  "Creates a custom mouse-cursor from an image"
  (setf *cursor* (sdl:blit-surface
		  (sdl:load-image "c:/te/cursor.bmp" :color-key-at color-key))
	*cursor-offset* offset)
  (sdl:show-cursor nil))


(defun mouse-move-direction (old-x x old-y y)
  "Get the direction the mouse is moving"
  (let ((vertical (cond
		    ((= old-y y) 'none)
		    ((> old-y y) 'up)
		    (t 'down)))
	
	(horizontal (cond ((= old-x x) 'none)
			  ((> old-x x) 'left)
			  (t 'right))))
    
    (vector horizontal vertical)))



;; Get mouse-wrap from sdl
(defun CFFI-init ()
  (cffi:define-foreign-library sdl
    (:windows "sdl.dll"))
  (cffi:use-foreign-library sdl)
  
  (cffi:defcfun ("SDL_WarpMouse" warp-mouse-at-*) :void
    (x :unsigned-short)
    (y :unsigned-short)))

;; Create alternative mouse-setter
(defun warp-mouse (point)
  (when (or (not (vectorp point)) (> (length point) 2))
    (error "Only accepts a vector of 2 cordinate points(x|y)"))
  (warp-mouse-at-* (elt point 0) (elt point 1)))
