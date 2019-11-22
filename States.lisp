;;;; Handles game states
(in-package #:sdl-helper-functions)

(defparameter *state* :menu) ; Default state is menu
(defparameter *states* '(:game :menu )) ; Default state list is menu

(defun set-state (state)
  "Sets the game state"
  (if (member state *states*)
      (setf *state* state)))

(defun check-state (state)
  (string= state *state*))

(defmacro with-state (state &body body)
  `(when (string= ,state *state*)
     ,@body))

(defun add-state (&rest states)
  "Add all the states to *States* variable"
  (dolist (state states)
    (push state *states*)))

#|| 
Create menu-creation,

menu creation take any number of objects in sequence.
Takes a sequence of either strings or surfaces, then do rect collision on it for mouse pointer.

Create ability to easily get if something is collided, and have been activated(some sort of state-system for the states)

||#
