(in-package :sdl-helper-functions)

(defparameter *scroll-boxes-list* nil) ; List of scroll boxes for the use of emptying when not clicked

(defclass scroll-box (rect)
  ((surface :accessor get-surface
	    :initarg :surface)
   (color :accessor get-box-color
	  :initarg :color)
   (direction :accessor get-box-dir
	      :initarg :dir
	      :documentation "what cordinate to scroll")
   (hitbox :accessor get-hitbox
	   :initarg :hitbox)
   (active :accessor is-active?
	   :initform nil
	   :documentation "if the mouse is currently engaged in this box")))


(defclass scroll-bar (rect)
  ((surface :accessor get-surface
	    :initarg :surface)
   (show :accessor show-scroll-bar?
	 :initarg :show)
   (scroll-box :accessor get-scroll-box :initarg :scroll-box)
   (hitbox :accessor get-hitbox :initarg :hitbox)
   (bar-color :accessor get-bar-color :initarg :bar-color)
   (box-color :accessor get-box-color :initarg :box-color)))


(defmethod change-surface ((object scroll-bar) &key alpha)
  (let* ((old-surface (get-surface object))
	 (surface (sdl:create-surface (sdl:width old-surface) (sdl:height old-surface) :alpha alpha)))

    ;; Surface for the scroll-box is the same as the scroll-bar
    (setf (get-surface (get-scroll-box object)) surface)
    (setf (get-surface object) surface)))



(defun get-scrollbox-hitbox (scroll-bar)
  (get-hitbox (get-scroll-box scroll-bar)))

(defun create-scroll-bar (x y w h &key  (show t) (bar-color (get-color darkgray)) (alpha 255)
				    (sb-x 0) (sb-y 0) (sb-w w) (sb-h h) (direction 'y)
				    (sb-color (get-color lightgray)) (sb-hitbox-color sb-color))
  "Creates a scroll-bar"
  (let* ((surface (sdl:create-surface w h :alpha alpha)))
    (make-instance 'scroll-bar :surface surface :w w :h h :x x :y y :bar-color bar-color  :show show
		   :scroll-box (create-scroll-box surface x y sb-x sb-y sb-w sb-h direction sb-color sb-hitbox-color)))) ;:box-color box-color)))


(defun add-to-scroll-box-list (scroll-box-instance)
  (setf *scroll-boxes-list* (cons scroll-box-instance *scroll-boxes-list*))
  scroll-box-instance)

(defun create-scroll-box (surface bar-x bar-y box-x box-y box-w box-h direction color hb-color)
  "Creates the box used for scrolling in a scrollbar"
  (add-to-scroll-box-list (make-instance 'scroll-box :surface surface :w box-w :h box-h :x box-x :y box-y :color color :dir direction
					 :hitbox (create-hitbox 'rect :x bar-x :y bar-y :w box-w :h box-h  ;(1+ box-h)
								:color hb-color))))


(defun draw-bar (scroll-bar)
  (sdl:draw-box-* 0 0 (w scroll-bar) (h scroll-bar) :surface (get-surface scroll-bar) :color (get-bar-color scroll-bar)))
  ;(when (scroll-bar-show? scroll-bar)
  ;  (sdl:draw-surface-at-* (get-surface scroll-bar) (x scroll-bar) (y scroll-bar))))

(defun draw-scroll-bar (scroll-bar) ;scroll-box)
  "Draw the scroll-bar and scroll-box boxes to the scroll-bar surface, then draws the surface to screen"
    (draw-bar scroll-bar)
    (draw-scroll-box (get-scroll-box scroll-bar)) ;(get-scroll-box scroll-bar))
  ;(draw-scroll-box scroll-box)
					;(sdl:draw-box-* (x scroll-box) (y scroll-box) (w scroll-box) (h scroll-box))
  (when (show-scroll-bar? scroll-bar)
    (sdl:draw-surface-at-* (get-surface scroll-bar) (x scroll-bar) (y scroll-bar))))


;; Rewrite so size is better created, more similar to text editors
(defun calculate-scroll-box-height (scroll-bar text-height line-amount &key (min-size 5))
  ""
      (let* ((max (round (h scroll-bar) text-height)) ;(round (/ (h scroll-bar) text-height )))
	     (hidden-vertical-lines (- line-amount max))
	     (height (if (<= hidden-lines 0)
			 0
			 (round (h scroll-bar) (if (= hidden-lines 1) 2 hidden-lines)
				#||(/ (h scroll-bar)
			      
			      ;; Allows scrolling if only one line is hidden
			      (if (= hidden-lines 1) 2 hidden-lines))||#))))
	      ;; If there are no lines hidden from view, make the scroll-box an height of 0
	(values (if (< height min-size) min-size height)
		max)))


(defun draw-scroll-box (scroll-box)
  "Draws the scroll-box"

  (sdl:draw-box-* (x scroll-box)
		  (y scroll-box) (w scroll-box) (h scroll-box)
		  :surface (get-surface scroll-box)
		  :color (get-box-color scroll-box)))

(defun scroll-box-active-mouse? (scroll-box )
  "Changes the active state of the scroll-box if mouse is clicked on it"
  (and (shf:mouse-collision-check (get-hitbox scroll-box)) (sdl:mouse-left-p)
       (setf (is-active? scroll-box) t)))

  
(defun out-of-bounds? (box-pos box-size bar-size)
  "Checks if the scroll box is out of bound of the bar"
  (cond ((<= box-pos 0)
	 'start)
	((>= (+ box-size box-pos) bar-size)
	 'end)))

(defun get-new-scroll-box-pos (mouse-pos bar-pos bar-size box-pos  box-size)
  "Returns the new position of the scroll box and it's hitbox in relative to the mouse position"
  (let ((bounds (out-of-bounds? box-pos box-size bar-size)))
    (cond ((and (string-equal bounds 'start) (< mouse-pos (+ bar-pos (round box-size 2 #||(/ box-size 2)||#))))
	   (values 0 bar-pos))
	  ((and (string-equal bounds 'end) (> mouse-pos (- (+ bar-size bar-pos) (round box-size 2 #||(/ box-size 2)||#))))
	   (values (- bar-size box-size)
		   (+ bar-pos box-pos)))
	  (t (values (- (- mouse-pos bar-pos) (round box-size 2 #||(/ box-size 2)||#))
		     (- mouse-pos (round box-size 2 #||(/ box-size 2)||#)))))))
#||
(defun set-scroll-box-pos (dir size scroll-bar mouse &aux (scroll-box (get-scroll-box scroll-bar))
					    (hitbox (get-hitbox scroll-box)))
  (setf (values (slot-value scroll-box dir) (slot-value hitbox dir))
	(get-new-scroll-box-pos mouse
				(slot-value scroll-bar dir) (slot-value scroll-bar size))))
||#



(defun fix-out-of-bounds (scroll-bar &aux (scroll-box (get-scroll-box scroll-bar))
					   (hitbox (get-hitbox scroll-box)))
  "Ensures the box isn't out of the bounds of scroll-bar"
                        ;; slot-value doesn't like the symbol returned by get-box-dir, so we add a new one
  (let* ((dir-slot (if (string-equal (get-box-dir scroll-box) 'y) 'y 'x))
	 (size-slot (if (string-equal dir-slot 'y) 'h 'w))
	 (bounds (out-of-bounds? (slot-value scroll-box dir-slot)
				 (slot-value scroll-box size-slot)
				 (slot-value scroll-bar size-slot))))

    (cond ((string-equal bounds 'start)
	   (setf (slot-value scroll-box dir-slot) 0)
	   (setf (slot-value hitbox dir-slot) (slot-value scroll-bar dir-slot)))
	  ((string-equal bounds 'end)
	   (setf (slot-value scroll-box dir-slot) (- (slot-value scroll-bar size-slot) (slot-value scroll-box size-slot)))
	   (setf (slot-value hitbox dir-slot) (+ (slot-value scroll-bar dir-slot) (slot-value scroll-box dir-slot)))))))

;; Rework scroll-box to use relative position
(defun scroll-box (scroll-bar &aux (scroll-box (get-scroll-box scroll-bar))
			       (hitbox (get-hitbox scroll-box)))
  "Call to automatically check for, and cause scrolling.
Destructivly change the positions of scroll-bar, scroll-box(and it's hitbox)
Returns nil if not scrolling"
  (scroll-box-active-mouse? scroll-box)
  (fix-out-of-bounds scroll-bar)
  
  (if (is-active? scroll-box)
      (cond ((string-equal (get-box-dir scroll-box) 'y)
	     (setf (values (y scroll-box) (y hitbox))
		   (get-new-scroll-box-pos (sdl:mouse-y) (y scroll-bar)  (h scroll-bar)
					   (y scroll-box) (h scroll-box))))
	    
	    ((string-equal (get-box-dir scroll-box) 'x)
	     (setf (values (x scroll-box) (x hitbox))
		   (get-new-scroll-box-pos (sdl:mouse-x) (y scroll-bar) (w scroll-bar)
					   (x scroll-box) (w scroll-box)))))
      nil))
