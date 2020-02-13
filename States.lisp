;;;; Handles game states
(in-package #:sdl-helper-functions)

(defparameter *state* :menu) ; Default state is menu
(defparameter *states* '(:game :quit :menu )) ; Default state list is menu
(defparameter *selected-index* 0)

(defun set-state (state)
  "Sets the game state"
  (if (member state *states*)
(setf *state* state)))
(defun check-state (state)
  (string= state *state*))

(defmacro with-state (state &body body)
  `(when (string= ',state *state*)
     ,@body))

(defun add-state (&rest states)
  "Add all the states to *States* variable"
  (dolist (state states)
    (push state *states*)))

(defun create-menu (items cordinates &key spacing (selection-style :keyboard) activation-keys selection-keys (mouse-select-mode t))
  (unless activation-keys
    (setf activation-keys
	  (ecase selection-style
	    (:keyboard-mouse '(:sdl-key-return :left))
	    (:keyboard '(:sdl-key-return))
	    (:mouse :left))))

  (when (and (or (string= selection-style :keyboard) (string= selection-style :keyboard-mouse)) (not selection-keys))
    (setf selection-keys '((next . :sdl-key-up) (previous . :sdl-key-down))))

  
  (cond ((typep (aref (aref items 0) 0) 'sdl:surface)
	 (draw-surface-main-menu items cordinates spacing  activation-keys selection-keys mouse-select-mode))
	(t (error "not supported item"))))



(defun keyboard-change-selection (selection-keys size)
  (cond ((and (> *selected-index*  0) (is-keys (cdr (assoc 'next selection-keys))))
	 (setf *not-pressing* nil)
	 (decf *selected-index*))
	((and (< *selected-index* size) (is-keys (cdr (assoc 'previous selection-keys))))
	 (setf *not-pressing* nil)
	 (incf *selected-index*))))


(defun set-active-menu-item (item i)
  (if (= i *selected-index*)
      (aref item 1)
      (aref item 0)))

(defun activate-item (item keys)
  (cond ((apply #'is-keys keys)
	 (setf *state* (aref item 2)))
	((is-mouse-keys keys)
	 (setf *state* (aref item 2)))))

(defun draw-surface-main-menu (items cordinates spacing activation-keys selection-keys mouse-hover-mode)
  "Automatically draw the main-menu surface items"
  (when *not-pressing*
    (keyboard-change-selection selection-keys (length items)))

  (iter (for i to (last-index items))
	(with item) (with x = (aref cordinates 0)) (with y = (aref cordinates 1))

	;; Change selected index to where the mouse pointer is, if mouse-hover-mode is active
	(when (and mouse-hover-mode (mouse-collision-check (vector x y (sdl:width (aref (aref items i) 0)) (sdl:height (aref (aref items i) 0)))))
	  (setf *Selected-index* i))

	
	(setf item (set-active-menu-item (aref items i) i))
	
	(if (= i *selected-index*)
	    (activate-item (aref items i) activation-keys))
	
	(sdl:draw-surface-at-* item x y)
	(incf y (+ (sdl:height item) (if spacing spacing 0))))
  (shf:draw-text (format nil "~a" activation-keys) #(0 50)))
