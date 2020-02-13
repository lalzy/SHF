(in-package :sdl-helper-functions)

(defun initialize-context-menu ()
  ;; Create context-menu state here!
  (add-state :context-menu)
  (let (menu-items previous-state key-mode (browse-keys (make-hash-table :size 4)) (selection-index 0) (item-amount 0))
    
    (defun create-background-box (width height)
      (let ((surface (sdl:create-surface width height)))
	(sdl:draw-box-* 0 0 width height :surface surface :color (get-color lightgray))
	surface))
    
    (defun clear-context-menu ()
      (set-state previous-state)
      (setf menu-items nil))

    (defun set-browse-keys (down up select cancel)
      (setf (gethash :down browse-keys) down
	    (gethash :up browse-keys) up
	    (gethash :select browse-keys) select
	    (gethash :cancel browse-keys) cancel
	    key-mode t))
    
    (defun create-context-menu (string-list &key (spacing 0) (x (sdl:mouse-x)) (y (sdl:mouse-y)) (font sdl:*default-font*))
      (unless (string= *state* :context-menu)
	(setf previous-state *state*))
      (set-state :context-menu)
      (dolist (string string-list)
	(incf item-amount)
	(push (vector string
		      (vector x y (w string font) (h string font))) menu-items)
	(incf y (+ spacing (h string font))))
      (if key-mode
	  (setf selection-index (1- item-amount))
	  (setf selection-index -1)))
    
    (defun move-index (check-function check-value reset amount)
      (if (funcall check-function selection-index check-value)
	  (setf selection-index reset)
	  (setf selection-index (+ selection-index amount))))

    (defun 1a (array)
      (aref array 0))
    
    (defun 2a (array)
      (aref array 1))

    (defun mouse-check ()
      (dolist (item menu-items)
	(when (mouse-collision-check (2a item)) (return-from mouse-check (1a item))))
      nil)

    (defun mouse-select-context-item (&key (select-keys '(:left)) (cancel-keys '(:right)))
      (cond ((is-mouse-keys cancel-keys)
	     (clear-menu))
	    ((is-mouse-keys select-keys)
	       (clear-menu (mouse-check)))))

    (defun clear-menu (&optional item)
      (setf menu-items nil
	    selection-index 0
	    item-amount 0
	    *state* previous-state)
      (when item item))
    
    (defun key-select-context-item (&aux (key (shf:get-pressed-key)))
      (cond ((string= (gethash :cancel browse-keys) key)
	     (clear-menu))
	    ((string= key (gethash :up browse-keys))
	     (move-index #'= (1- item-amount) 0 1)
	     nil)
	    ((string= key (gethash :down browse-keys))
	     (move-index #'= 0 (1- item-amount) -1)
	     nil)
	    ((string= key (gethash :select browse-keys))
	     (clear-menu (1a (nth selection-index menu-items))))))
	    


    (defun run-context-menu (&optional (mouse-on t))
      (let ((index 0))
	(dolist (item menu-items)
	  ;; Change selection if mouse move and mouse is on
	  (when (and mouse-on (mouse-moved) (mouse-collision-check (2a item))) (setf selection-index index))
	  (draw-context-menu item index)
	  (incf index))))
    
    (defun draw-context-menu (item index)
      (if (= index selection-index)
	  (shf:draw-text (1a item) (2a item) :default-color (shf:get-color blue))
	  (shf:draw-text (1a item) (2a item))))))
