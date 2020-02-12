(in-package :sdl-helper-functions)

(defun get-font-sizes (text &optional (font sdl:*default-font*))
  (values (sdl:get-font-size text :size :w :font font) (sdl:get-font-size text :size :h :font font)))

(defun initialize-context-menu (&optional menu-offset-x menu-offset-y)
  ;; Create context-menu state here!
  (add-state 'context-menu)
  (let (menu-items menu-position previous-state select-box background (text-color (shf:get-color black)) (select-color (shf:get-color lightgray)))
    
    (defun create-background-box (width height)
      (let ((surface (sdl:create-surface width height)))
	(sdl:draw-box-* 0 0 width height :surface surface :color (get-color lightgray))
	surface))
    
    (defun set-text-color (color) (setf text-color color))
    (defun set-select-color (color) (setf select-color color))
    
    (defun create-context-menu (items-list &key  (offset-x 0) (offset-y 0) (x (sdl:mouse-x)) (y (sdl:mouse-y)) (spacing 0) (font sdl:*default-font*) background-image foreground-image)
      ;; Set position of the menu
      ;; Set state to context-menu
      ;; Create position of individual elements?
      ;;   vector> x y w h
      (incf x offset-x) (incf y offset-y)
      (setf previous-state *state*)
      (set-state 'context-menu)
      (let ((total-height offset-y)
	    (max-width 0))
	(setf menu-position (vector x y))
	(dolist (item items-list)
	  (multiple-value-bind (width height) (get-font-sizes item font)
	    (push (vector item (vector x y width (1- (+ height spacing)))) menu-items)
	    (incf y (+ spacing height))
	    (incf total-height (+ spacing height))
	    (when (> width max-width) (setf max-width width))))

	(setf background
	      (if background-image
		  background-image
		  (create-background-box max-width total-height)))))
    
    (defun select-context-item ()
      (setf menu-items nil)
      (set-state previous-state))
    
    (defun draw-context-menu (&optional selection-box-color)
      ;(break (format nil "~a" background))
      (sdl:draw-surface-at background menu-position)
      (dolist (item menu-items)
	(let ((color (if (mouse-collision-check (aref item 1))
			 (progn
			   (when selection-box-color
			     (sdl:draw-box-* (x (aref item 1)) (y (aref item 1)) (w background) (h (aref item 1)) :color selection-box-color) (aref item 1))
			   (get-color blue))
			 (get-color black))))
	(shf:draw-text (format nil "~a" (aref item 0)) (aref item 1) :default-color color))))))





#|
  when rightclick call create-context-menu.
     > set menu at mouse-position
     > loop through and position each element subsequently from options-list

   create-menu
   

|#
