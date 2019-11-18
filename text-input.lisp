;;;; sdl-helper-functions.lisp
;;;  Test area - x=320 y=28 - x=780 y=345
;;;
;;; - Create default collision detection on text-field?

(in-package #:sdl-helper-functions)


(defun create-new-line (text-list)
  (if text-list
      (push nil (cdr (last text-list)))))

(defun append-character-to-string (text new-char)
  (concatenate 'string text (string new-char)))

(defun add-character-to-textfield (textfield index text new-char)
  (if (>= index 0)
      (setf (elt (get-text textfield) index) (append-character-to-string text new-char))
      (setf (get-text textfield) (list (string new-char)))))

(defun remove-character-from-textfield (textfield index)
  (when (>= index 0)
    (let* ((text (elt (get-text textfield) index))
	   (length (length text)))
    
      (if (> length 0)
	  (setf (elt (get-text textfield) index) (subseq text 0 (1- length)))
	  (setf (get-text textfield) (remove-if #'(lambda (x) t) (get-text textfield) :from-end t :count 1))))))

(defun is-mod-key (sdl-key list-of-mod-keys)
  (mapcar #'(lambda (key) (if (string-equal sdl-key key)
			      (return-from is-mod-key t))) list-of-mod-keys)
  nil)



(defun input-text-to-field (textfield &key multi-lines (list-of-mod-keys
					 		`(:SDL-KEY-LSHIFT
							  :SDL-KEY-RSHIFT
							  :sdl-key-rctrl
							  :sdl-key-lctrl
							  ;; adds enter\return key to special mod key
							  ,(unless multi-lines :sdl-key-return))))
  "Adds text to the textfield"
  (when (is-active? textfield)
    (let (sdl-key unicode-key)
      (setf (values sdl-key unicode-key) (get-pressed-key))

      (unless (is-mod-key sdl-key list-of-mod-keys)
	(let* ((text-list (get-text textfield))
	       (text (first (last text-list))))

	  (cond ((and multi-lines (string-equal sdl-key :sdl-key-return))
		 (setf text-list (create-new-line text-list)))
		((string-equal sdl-key :sdl-key-backspace)
		 (remove-character-from-textfield textfield (1- (length text-list))))
		
		(t (add-character-to-textfield textfield (1- (length text-list)) text unicode-key))))))))
