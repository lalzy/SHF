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
  nil)

;; Change to check if old-obj is rect
(defmethod collision-check ((object1 rect) (object2 rect) &optional old-obj);((object1 hitbox-rect) (object2 hitbox-rect))
  (if (typep old-obj 'rect) ; Ensures old-object is same as object1
      (rect-collision-check (get-range old-obj object1) object2)
      (rect-collision-check object1 object2)))

;; Change to check if old-obj is rect
(defmethod collision-check ((object1 sdl:surface) (object2 rect) &optional old-obj);((object1 hitbox-rect) (object2 hitbox-rect))
  (if (typep old-obj 'rect) ; Ensures old-object is same as object1
      (rect-collision-check (get-range old-obj object1) object2)
      (rect-collision-check object1 object2)))

;; Change to check if old-obj is rect
(defmethod collision-check ((object1 sdl:surface) (object2 sdl:surface) &optional old-obj);((object1 hitbox-rect) (object2 hitbox-rect))
  (if (typep old-obj 'rect) ; Ensures old-object is same as object1
      (rect-collision-check (get-range old-obj object1) object2)
      (rect-collision-check object1 object2)))

(defmethod collision-check ((object1 rect) (object2 circle) &optional old-obj);((object1 hitbox-rect) (object2 hitbox-circle))
  (if (typep old-obj 'rect)
      (rect-circle-collision-check (get-range old-obj object1) object2)
      (rect-circle-collision-check object1 object2)))

(defmethod collision-check ((object1 sdl:surface) (object2 circle) &optional old-obj);((object1 hitbox-rect) (object2 hitbox-circle))
  (if (typep old-obj 'rect)
      (rect-circle-collision-check (get-range old-obj object1) object2)
      (rect-circle-collision-check object1 object2)))

(defmethod collision-check ((object1 circle) (object2 rect) &optional old-obj);((object1 hitbox-circle) (object2 hitbox-rect))
  (if (typep old-obj 'circle) ; Ensures old-object is same as object1

      ;; If object1 and old-object are in the same position, do circle collision, if not do range collision.
      (if (object-same-pos-p object1 old-obj)
	  (rect-circle-collision-check object2 object1)
	  (rect-collision-check (get-range old-obj object1) object2))
      (rect-circle-collision-check object2 object1)))

(defmethod collision-check ((object1 circle) (object2 sdl:surface) &optional old-obj);((object1 hitbox-circle) (object2 hitbox-rect))
  (if (typep old-obj 'circle) ; Ensures old-object is same as object1

      ;; If object1 and old-object are in the same position, do circle collision, if not do range collision.
      (if (object-same-pos-p object1 old-obj)
	  (rect-circle-collision-check object2 object1)
	  (rect-collision-check (get-range old-obj object1) object2))
      (rect-circle-collision-check object2 object1)))

(defmethod collision-check ((object1 circle) (object2 circle) &optional old-obj);((object1 hitbox-circle) (object2 hitbox-circle))
  (if (typep old-obj 'rect)
      (if (object-same-pos-p object1 old-obj) 
	  (circle-collision-check object1 object2)
	  (rect-circle-collision-check (get-range old-obj object1) object2))
      (circle-collision-check object1 object2)))



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
  
(defmethod get-range (old-obj object &aux sx sy ex ey))

(defmethod get-range ((old-obj circle) (object circle) &aux (pos (get-range-positions old-obj object)))
  "Get the start and end position(size) of the range, only registers about half of the circle,
so end position uses standard non-range collision"
  (make-instance 'sdl:rectangle :x (elt pos 0) :y  (elt pos 1)
 ; (make-instance 'rect :x (- (elt pos 0) (r object)) :y (- (elt pos 1) (r object))
		 :w (+ (elt pos 2) (r object)) :h (+ (elt pos 3)  (r object)))) 
	;	 :w (+ (elt pos 2) (* 2 (r object))) :h (+ (elt pos 3) (* 2 (r object)))))

;make generic documented
(defmethod get-range ((old-obj rect) (object rect)) (get-range-rect-helper old-obj object))
(defmethod get-range ((old-obj sdl:surface) (object sdl:surface)) (get-range-rect-helper old-obj object))
(defmethod get-range ((old-obj sdl:surface) (object rect)) (get-range-rect-helper old-obj object))
(defmethod get-range ((old-obj rect) (object sdl:surface)) (get-range-rect-helper old-obj object))
(defun get-range-rect-helper ((old-obj rect) (object sdl:rectangle) &aux (pos (get-range-positions old-obj object)))
  "Get the start and end position(size) of the range"
  (make-instance 'sdl:rectangle :x (elt pos 0) :y (elt pos 1)
		 :w (+ (w object) (elt pos 2)) :h (+ (h object) (elt pos 3))))


(defun object-same-pos-p (object1 object2)
  "Checks if object1 and object2 are at the same position, a.k.a, not moved"
  (if (and (= (x object1) (x object2)) (= (y object1) (y object2)))
      t
      nil))


(defgeneric mouse-collision-check (object &optional mouse)
  (:documentation "Collision checking between object and the current position of the mouse"))

(defmethod mouse-collision-check ((object rect) &optional (mouse (vector (sdl:mouse-x) (sdl:mouse-y))))
  (and (pixel-rect-collision-check (x object) (y object) (w object) (h object) (aref mouse 0) (aref mouse 1))))

(defmethod mouse-collision-check ((object sdl:surface) &optional (mouse (vector (sdl:mouse-x) (sdl:mouse-y))))
  (and (pixel-rect-collision-check (x object) (y object) (w object) (h object) (aref mouse 0) (aref mouse 1))))

(defmethod mouse-collision-check ((object vector) &optional (mouse (vector (sdl:mouse-x) (sdl:mouse-y))))
  (and (pixel-rect-collision-check (aref object 0) (aref object 1) (aref object 2) (aref object 3) (elt mouse 0) (elt mouse 1))))


(defmethod mouse-collision-check ((object circle) &optional (mouse (vector (sdl:mouse-x) (sdl:mouse-y))))
  (if (pixel-circle-collision-check (x object) (y object) (r object) (elt mouse 0) (elt mouse 1))
      object
      nil))
#||
(defmethod mouse-collision-check ((object rect) &optional (mouse (vector (sdl:mouse-x) (sdl:mouse-y))))
  (if (pixel-rect-collision-check (x object) (y object) (w object) (h object) (elt mouse 0) (elt mouse 1))
      object
      nil))
||#

(defun mouse-text-collision (text x y  &key (mouse-x (sdl:mouse-x)) (mouse-y (sdl:mouse-y)) (font sdl:*default-font*) (name text))
  ""
  (if (pixel-rect-collision-check
       x y
       (w text font)
       (h text font)
       ;(sdl:get-font-size text :size :w :font font)
       ;(sdl:get-font-size text :size :h :font font)
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

(defmethod edge-collision-check ((object sdl:rectangle) &optional (beyond nil))
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
  



