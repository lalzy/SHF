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
  (cond ((typep (aref (aref items 0) 0) 'sdl:surface)
	 (draw-surface-main-menu items cordinates spacing selection-function))
	(t (error "not supported item"))))

(defun is-active-menu-item? (item x y selection-function &aux (unactive-object (aref item 0)))
  (if (mouse-collision-check (vector x y (sdl:width unactive-object) (sdl:height unactive-object)))
      (progn (when (apply (first selection-function) (rest selection-function))
	       (setf *state* (aref item 2)))
	     (aref item 1))
      (aref item 0)))


(defun draw-surface-main-menu (items cordinates spacing selection-function)
  "Automatically draw the main-menu surface items"
  ;;(loop for i to (1- (length items))
  (iter (for i to (last-index items))
	(with item) (with x = (aref cordinates 0)) (with y = (aref cordinates 1))
       (setf item (is-active-menu-item? (aref items i)  x y selection-function))
       (sdl:draw-surface-at-* item x y)
       (incf y (+ (sdl:height item) (if spacing spacing 0)))))
