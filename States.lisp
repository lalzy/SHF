;;;; Handles game states
(in-package #:sdl-helper-functions)

(defparameter *state* :menu) ; Default state is menu
(defparameter *states* '(:game :quit :menu )) ; Default state list is menu

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

(defun create-menu (items cordinates &key spacing (selection-function '(sdl:mouse-left-p)))
  (cond ((typep (elt (first items) 0) 'sdl:surface)
	 (draw-surface-main-menu items cordinates spacing selection-function))
	(t (error "not supported item"))))
#||
(defun list-or-array-p (item)
  (if (= (length item) 2)
      (if (arrayp (elt item 0))
	  t)
      (or (listp item) (arrayp item))))
||#

(defun is-active-menu-item? (item x y selection-function &aux (unactive-object (elt item 0)))
  (if (mouse-collision-check (vector x y (sdl:width unactive-object) (sdl:height unactive-object)))
      (progn (when (apply (first selection-function) (rest selection-function))
	       (setf *state* (elt item 2)))
	     (elt item 1))
      (elt item 0)))


(defun draw-surface-main-menu (items cordinates spacing selection-function)
  "Automatically draw the main-menu surface items"
  (loop for i to (1- (length items))
     with item and  x = (elt cordinates 0) and y = (elt cordinates 1)
     do
       (setf item (is-active-menu-item? (elt items i)  x y selection-function))
       (sdl:draw-surface-at-* item x y)
       (incf y (+ (sdl:height item) (if spacing spacing 0)))))
