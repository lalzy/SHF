;;;; Handles game states

(in-package #:sdl-helper-functions)

(defparameter *state* 'menu) ; Default state is menu
(defparameter *states* '(menu)) ; Default state list is menu

(defun set-state (state)
  "Sets the game state"
  (setf *state* state))

(defun check-state (state function &rest arguments)
  "Checks if passed state is the current state, if it is calls passed state function"
  (when (string= state *state*)
    (if arguments 
	(funcall function arguments)
	(funcall function))))

(defun create-states (states)
  "Add all the states to *States* variable"
  (dolist (state states)
    (push state *states*)))


(defun create-menu (menu-items &key (start-position #(0 0)) (font sdl:*default-font*))
  "Options become both text options, and automatically states"
  (create-states menu-items)
  (show-menu menu-items start-position font))


(defun show-menu (menu-items position font)
  (let ((text (first menu-items)))
    (draw-text (format nil "~a" text) position)
    (draw-text (format nil "size = ~a" (sdl:get-font-size text :size :w)) (vector (elt position 0) (+ 30 (elt position 0))))))
  ;(let ((text (make-text-surface (first menu-items))))
  ;    (sdl:draw-surface-at (get-text-surface text) position)))


; Calculate text-collision, get x\y pos,  then font :size :w, font :size :h,  which is a rect of size of font and text