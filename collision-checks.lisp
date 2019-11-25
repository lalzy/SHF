(in-package #:sdl-helper-functions)

;;; ToDo: no hitbox use sprite as rect collision
;;;
;;;       edge-circle-collision - create that
;;;


(defmacro loop-hitbox (object var &body body)
  "Loop trough a sprite instance's hitboxes using passed var"
  `(dolist (,var (get-sprite-hitboxes ,object))
     ,@body))

(defgeneric collision-check (object1 object2 &optional old-obj)
  (:documentation "Collision Detection Methods, takes either an sprite instance(with hitboxes), or an hitbox instance. 
Optionally if old-pos(a position vector) is set will do a range collision check, rather than onpoint"))

(defmethod collision-check (object1 object2 &optional old-obj)
  (get-collision object1 object2 old-obj))


(defmethod collision-check ((object1 sprite-class) (object2 sprite-class) &optional old-obj)
  (let ((col nil)
	(h1 nil)
	(h2 nil))
    (loop-hitbox object1 h1
	 (loop-hitbox object2 h2
	      ;; If the old-object is a sprite, loop through all of it's hitboxes as well
	      (if (typep old-obj 'sprite-class)
		  (let ((h3 nil))
		    (loop-hitbox old-obj h3
			 (let ((res (get-collision h1 h2 h3)))
			   (when res (push res col)))))
		  (let ((res (get-collision h1 h2 old-obj)))
		    (when res (push res col))))))
    col))


#||
(defmethod collision-check ((object1 sprite-class) (object2 sprite-class) &optional old-obj)
  (let ((col nil)
	(h1 nil)
	(h2 nil))
    (loop-hitbox object1 h1
	 (loop-hitbox object2 h2
	      (let ((res (get-collision h1 h2 old-obj)))
		(when res (push res col)))))
    col))
||#

(defmethod collision-check ((object1 sprite-class) object2 &optional old-obj)
  (let ((col nil)
	(h1 nil))
    (loop-hitbox object1 h1
	 
	 (if (typep old-obj 'sprite-class)
	     (let ((h2 nil))
	       (loop-hitbox old-obj h2
		    (let ((res (get-collision h1 object2 h2)))
		      (when res (push res col)))))
	     (let ((res (get-collision h1 object2 old-obj)))
	       (when res
		 (push res col)))))
    col))

(defmethod collision-check (object1 (object2 sprite-class) &optional old-obj)
  (let ((col nil)
	(h2 nil))
    (loop-hitbox object2 h2
	 (let ((res (get-collision object1 h2 old-obj)))
	   (when res
	     (push res col))))
    col))


(defmethod get-range (old-obj object &aux sx sy ex ey))

(defun get-range-positions (old-obj object &aux sx ex sy ey)
  "Get the starting and ending position between the old and new object"
  (if (< (x old-obj) (x object))
      (setf sx (x old-obj) ex (x object))
      (setf ex (x old-obj) sx (x object)))
  
  (if (< (y old-obj) (y object))
      (setf sy (y old-obj) ey (y object))
      (setf ey (y old-obj) sy (y object)))

  ;; reduce the end position by the starting position to make the range between start\end.
  (setf ex (- ex sx)
	ey (- ey sy))
  (list sx sy ex ey))
  
(defmethod get-range ((old-obj circle) (object circle) &aux (pos (get-range-positions old-obj object)))
  "Get the start and end position(size) of the range, only registers about half of the circle,
so end position uses standard non-range collision"
  (make-instance 'rect :x (elt pos 0) :y  (elt pos 1)
 ; (make-instance 'rect :x (- (elt pos 0) (r object)) :y (- (elt pos 1) (r object))
		 :w (+ (elt pos 2) (r object)) :h (+ (elt pos 3)  (r object)))) 
	;	 :w (+ (elt pos 2) (* 2 (r object))) :h (+ (elt pos 3) (* 2 (r object)))))
	  
(defmethod get-range ((old-obj rect) (object rect) &aux (pos (get-range-positions old-obj object)))
  "Get the start and end position(size) of the range"
  (make-instance 'rect :x (elt pos 0) :y (elt pos 1)
		 :w (+ (w object) (elt pos 2)) :h (+ (h object) (elt pos 3))))


(defmethod get-collision (object1 object2 old-obj)) ;; Does nothing

(defun object-same-pos-p (object1 object2)
  "Checks if object1 and object2 are at the same position, a.k.a, not moved"
  (if (and (= (x object1) (x object2)) (= (y object1) (y object2)))
      t
      nil))

(defgeneric get-collision (object1 object2 old-obj)
  (:documentation "Ensures the correct collision form is done depending on what type of object is passed(rectangle or Circle)"))

;; Change to check if old-obj is rect
(defmethod get-collision ((object1 rect) (object2 rect) old-obj);((object1 hitbox-rect) (object2 hitbox-rect))
  (if (typep old-obj 'rect) ; Ensures old-object is same as object1
      (rect-collision-check (get-range old-obj object1) object2)
      (rect-collision-check object1 object2)))

(defmethod get-collision ((object1 rect) (object2 circle) old-obj);((object1 hitbox-rect) (object2 hitbox-circle))
  (if (typep old-obj 'rect)
      (rect-circle-collision-check (get-range old-obj object1) object2)
      (rect-circle-collision-check object1 object2)))

(defmethod get-collision ((object1 circle) (object2 rect) old-obj);((object1 hitbox-circle) (object2 hitbox-rect))
  (if (typep old-obj 'circle) ; Ensures old-object is same as object1

      ;; If object1 and old-object are in the same position, do circle collision, if not do range collision.
      (if (object-same-pos-p object1 old-obj)
	  (rect-circle-collision-check object2 object1)
	  (rect-collision-check (get-range old-obj object1) object2))
      (rect-circle-collision-check object2 object1)))


(defmethod get-collision ((object1 circle) (object2 circle) old-obj);((object1 hitbox-circle) (object2 hitbox-circle))
  (if (typep old-obj 'rect)
      (if (object-same-pos-p object1 old-obj) 
	  (circle-collision-check object1 object2)
	  (rect-circle-collision-check (get-range old-obj object1) object2))
      (circle-collision-check object1 object2)))


(defun get-collision-hitbox-name (name-check list)
  "Get the selected hitbox by name if it exists inside the list of hitbox collisions"
  (block function
    (let ((names nil))
      (dolist (obj-list list)
	(dolist (obj obj-list)
	  (when (equalp name-check (shf:get-hitbox-name obj))
	    (return-from function name-check)))))))

(defgeneric mouse-collision-check (object &optional mouse)
  (:documentation "Collision checking between object and the current position of the mouse"))

(defmethod mouse-collision-check ((object vector) &optional (mouse (vector (sdl:mouse-x) (sdl:mouse-y))))
  (and (pixel-rect-collision-check (elt object 0) (elt object 1) (elt object 2) (elt object 3) (elt mouse 0) (elt mouse 1))))

(defmethod mouse-collision-check ((object sprite-class) &optional (mouse (vector (sdl:mouse-x) (sdl:mouse-y))))
  (let ((col nil)
	(hitbox nil))
    (loop-hitbox object hitbox ; Loop through and do collision check on the individual hitboxes of the sprite
	 (let ((res nil))
	   (if (string= (type-of hitbox) 'hitbox-rect)
	       (setf res (pixel-rect-collision-check (x hitbox) (y hitbox) (w hitbox)
						     (h hitbox) (elt mouse 0) (elt mouse 1)))
	       (setf res (pixel-circle-collision-check (x hitbox) (y hitbox) (r hitbox)
					     (elt mouse 0) (elt mouse 1))))
		 (when res
		   (push hitbox col))))
    (list col)))

(defmethod mouse-collision-check ((object circle) &optional (mouse (vector (sdl:mouse-x) (sdl:mouse-y))))
  (if (pixel-circle-collision-check (x object) (y object) (r object) (elt mouse 0) (elt mouse 1))
      object
      nil))

(defmethod mouse-collision-check ((object rect) &optional (mouse (vector (sdl:mouse-x) (sdl:mouse-y))))
  (if (pixel-rect-collision-check (x object) (y object) (w object) (h object) (elt mouse 0) (elt mouse 1))
      object
      nil))

(defun mouse-text-collision (text x y  &key (mouse-x (sdl:mouse-x)) (mouse-y (sdl:mouse-y)) (font sdl:*default-font*) (name text))
  ""
  (if (pixel-rect-collision-check
       x y
       (sdl:get-font-size text :size :w :font font)
       (sdl:get-font-size text :size :h :font font)
       mouse-x mouse-y)
      name
      nil))

;(defmethod mouse-collision-check (object)
;  nil)

(defun pixel-circle-collision-check (x y r px py)
  "pixel collision detection between pixel point and a circle"
  (let ((deltax (- x px ))
	(deltay (- y py)))
    (if (<= (+ (* deltax deltax) (* deltay deltay)) (* r r))
	t
	nil)))

(defun pixel-rect-collision-check (x y w h px py)
  "pixel collision detection between pixel point and a rect"
  (if (and (<= px (+ x w))
	   (>= px x)
	   (<= py (+ y h))
	   (>= py y))
      t
      nil))


(defun rect-circle-collision-check (rect circle)
  "Input: A rect and a cirlce object"
  (let ((deltax (- (x circle) (max (x rect) (min (x circle) (+ (x rect) (w rect))))))
	(deltay (- (y circle) (max (y rect) (min (y circle) (+ (y rect) (h rect)))))))
    (if (< (+ (* deltax deltax) (* deltay deltay)) (* (r circle) (r circle)))
	(list rect circle)
	nil)))

(defun rect-collision-check (rect1 rect2)
  "input: two rect objects"
    (if (and (< (x rect1) (+ (x rect2) (w rect2)))
	     (> (+ (x rect1) (w rect1)) (x rect2))
	     (< (y rect1) (+ (y rect2) (h rect2)))
	     (> (+ (y rect1) (h rect1)) (y rect2)))
	(progn
	  (format t "r1 = ~a, r2 = ~a~%" (x rect1) (x rect2))
	  (list rect1 rect2)
	  )
	nil))

(defun circle-collision-check (circle1 circle2)
  "Input: two circle objects"
  (let* ((distx (- (x circle2) (x circle1)))
	 (disty (- (y circle2) (y circle1)))
	 (radius (+ (r circle1) (r circle2))))
    (if (< (+ (* distx distx) (* disty disty)) (* radius radius))
	t
	nil)))
  



(defgeneric edge-collision-check (object &optional beyond)
  (:documentation "Checks if the [object] collides with the edges of the window, 
if beyond is set it'll only colide if the object is past the window"))

(defmethod edge-collision-check (object &optional (beyond nil))
  "No collision if not of a valid shape"
  nil)

(defmethod edge-collision-check ((object rect) &optional (beyond nil))
  "Edge collision for a rectangle instance"
  (edge-rect-collision object beyond))

(defmethod edge-collision-check ((object circle) &optional (beyond nil))
  "Edge collision for a rectangle instance"
  (edge-circle-collision object beyond))

(defun get-edge-dir (object dir &key (beyond nil))
  "Check if object is coliding with the chosen direction"
  (member dir (edge-collision-check object beyond) :test #'string=))
 ; (member dir (edge-rect-collision object beyond) :test #'string=))

(defun edge-rect-collision (rect beyond &aux (ex 0) (ey 0) (ew *width*) (eh *height*) (col nil))
  "Checks if rectangle collides with the edge of the window.
If beyond is set, will only be considered collision if object is past window"
  (when beyond
    (setf ex (- ex (w rect))
	  ey (- ey (h rect))
	  ew (+ ew (w rect))
	  eh (+ eh (h rect))))
  
   (when (<= (x rect) ex)
     (push 'left col))
   (when (>= (+ (x rect) (w rect)) ew)
    (push 'right col))
   (when (<= (y rect) ey)
    (push 'top col))
   (when (>= (+ (y rect) (h rect)) eh)
     (push 'bottom col))
   col)

(defun edge-circle-collision (circle beyond &aux (ex 0) (ey 0) (ew *width*) (eh *height*) (col nil))
  "Checks if circle collides with the edge of the window"
  (if beyond
      (progn
	(when (<= (x circle) (- ex (r circle)))
	  (push 'left col))
	(when (>= (- (x circle) (r circle)) ew)
	  (push 'right col))
	(when (<= (y circle) (- ey (r circle)))
	  (push 'top col))
	(when (>= (- (y circle) (r circle)) eh)
	  (push 'bottom col)))
      

      (progn
	(when (<= (x circle) (+ ex (r circle)))
	  (push 'left col))
	(when (>= (+ (x circle) (r circle)) ew)
	  (push 'right col))
	(when (<= (y circle) (+ (r circle) ey))
	  (push 'top col))
	(when (>= (+ (y circle) (r circle)) eh)
	  (push 'bottom col))))
  col)
  



