;;;; sdl-helper-functions.lisp
;;;  Test area - x=320 y=28 - x=780 y=345
;;;
;;; - Create default collision detection on text-field?

(in-package #:sdl-helper-functions)


(defun input-text-to-field (textfield)
  (when (is-active? textfield)
    (let (k u)
      (setf (values k u) (get-pressed-key))

      
      
      (unless (or (string-equal k :sdl-key-RSHIFT) (string-equal k :sdl-key-LSHIFT))
	(let* ((text-list (get-text textfield))
	       (text (first (last text-list)))
	       (length (length (get-text textfield))))
	  (if (string-equal k :sdl-key-return)
	      (progn
		(push nil (cdr (last text-list)))
		(setf (get-text textfield) text-list)
		(if (= (get-line-amount textfield) 0)
		    (incf (get-line-amount textfield) 2)
		    (incf (get-line-amount textfield)))
		)
	      (if (> length 0)
		  (setf (elt (get-text textfield) (1- length)) (concatenate 'string text (string u)))
		  (setf (get-text textfield) (list (string u))))))))))
