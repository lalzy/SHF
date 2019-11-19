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

(defun check-keypad-num (sdl-key)
  "Check if we are pressing the numpad keys and return the numbers,
needed incase of numlock being off, for some reason sdl still
sees it as numbers, but the unicode characters are not numbers."
  (case sdl-key
    (:sdl-key-kp0 #\0)
    (:sdl-key-kp1 #\1)
    (:sdl-key-kp2 #\2)
    (:sdl-key-kp3 #\3)
    (:sdl-key-kp4 #\4)
    (:sdl-key-kp5 #\5)
    (:sdl-key-kp6 #\6)
    (:sdl-key-kp7 #\7)
    (:sdl-key-kp8 #\8)
    (:sdl-key-kp9 #\9)
    (t nil)))

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
					 		`(:SDL-KEY-LSHIFT  :SDL-KEY-RSHIFT :sdl-key-rctrl :sdl-key-lsuper
							  :sdl-key-rsuper :sdl-key-ralt :sdl-key-lalt :sdl-key-menu
							  :sdl-key-tab :sdl-key-pageup  :sdl-key-pagedown  :sdl-key-insert
							  :sdl-key-home  :sdl-key-end  :sdl-key-escape  :sdl-key-f1
							  :sdl-key-f2 :sdl-key-f3   :sdl-key-f4 :sdl-key-f5
							  :sdl-key-f6  :sdl-key-f7  :sdl-key-f8 :sdl-key-f9
							  :sdl-key-f10  :sdl-key-f11 :sdl-key-f12  :sdl-key-f13
							  :sdl-key-f14  :sdl-key-f15  :sdl-key-rmeta  :sdl-key-lmeta
							  :sdl-key-mode :sdl-key-compose :sdl-key-help  :sdl-key-print
							  :sdl-key-numlock   :sdl-key-power  :sdl-key-undo  :sdl-key-left
							  :sdl-key-right  :sdl-key-up   :sdl-key-KP-Enter  :sdl-key-down
							  :sdl-key-delete  :sdl-key-scrolllock  :sdl-key-pause  :sdl-key-PRINTSCREEN
							  :sdl-key-delete  :SDL-key-capslock :sdl-key-lctrl
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
		(t (add-character-to-textfield textfield (1- (length text-list)) text (or (check-keypad-num sdl-key) unicode-key)))))))))
