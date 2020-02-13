(in-package :sdl-helper-functions)

(defparameter *key-pressed-code* nil) ; List of current key pressed(first element is sdl key, second is unicode code)
(defparameter *key-pressed-state* nil) ; A list of the current key state
(defparameter *not-pressing* t) ;; Stops us from beng able to keep a key pressed down

(defparameter *mouse-move-direction* #(none none)) ;direction mouse is currently moving

(defparameter *cursor* nil) ; Custom mouse cursor
(defparameter *cursor-offset* nil)
(defparameter *Current-mouse-button* nil)
(defparameter *mouse-state* 0)

(defun is-mouse-key (key)
  "Checks if a mouse button has been pressed"
  (when (numberp *current-mouse-button*)
    (case key
      (:left
       (= *current-mouse-button* 1))
      (:middle
       (= *current-mouse-button* 2))
      (:right
       (= *current-mouse-button* 3))
      (:wheel-up
       (= *current-mouse-button* 4))
      (:wheel-down
       (= *current-mouse-button* 5))
      (:x1
       (= *current-mouse-button* 6))
      (:x2
       (= *current-mouse-button* 7)))))

(defun is-mouse-keys (keys)
  (if (listp keys)
      (dolist (key keys)
	(when (is-mouse-key key) (return t)))
      (is-mouse-key keys)))

(defun mouse-moved ()
  (every #'(lambda (x) (not (string= x 'none))) *mouse-move-direction*))

;Rename to key a
(defun is-keys (&rest keys)
  "Take a list of keys and return true if any one of them has been pressed"
  (when keys
    (find-if #'(lambda (key) (member key *key-pressed-state*)) keys)))

(defun is-all-keys (&rest keys)
  "Takes a list of keys and check if all are pressed"
  (when keys
    (notany #'null (mapcar #'(lambda (key) (member key *key-pressed-state*)) keys))))

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

(defun create-cursor (img-src &optional (offset #(0 0)) (color-key #(0 0)))
  "Creates a custom mouse-cursor from an image"
  (unless (or (typep img-src 'pathname) (stringp img-src))
    (error "Cursor needs to be either a string, or a pathname!"))
  
  (sdl:show-cursor nil)
  (setf *cursor*
	(sdl:blit-surface
	 (sdl:load-image img-src :color-key-at color-key))
	*cursor-offset* offset))
  


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



