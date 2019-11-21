;;;; sdl-helper-functions.lisp

(in-package #:shf-error-handling)

;; Make a dynamic try-retry error

(defparameter *quit-on-error* t)

(defun quit ()
   #+:ccl (ccl:quit)
   #+:sbcl (sb-ext:exit))
   
   
(defun error-message (text &key (title "Error!") (button :ok))
  "Opens up an windows error message box"
  (ftw:message-box :text (format nil "~a" text) :icon :error  :caption (format nil "~a" title)  :button button))

(defmacro try-retry (try-func &key (cancel '(quit)) ;'(ccl:quit)) 
				(text nil) (title nil));&aux (attempt (gensym)) (result (gensym)) ())
  "A try-retry error system. Takes a function to attempt an operation on, 
  if an error occurs will throw an retry-cancel message box that'll attempt the same operation everytime retry is pressed
  Text is the error text, title is the message box title, cancel is what to do if it fails, default is exit"
  (alexandria:with-gensyms (return-data attempt result)
    `(labels ((,attempt () ; Used for recursion
		(let* ((,return-data nil)
		       (,result 
			(handler-case 
			    ;; Ensures a symbol is returned if successfull
			    (progn
			      (setf ,return-data ,try-func)
			      'success)
			  (error (e)
			    
			    ;; Uses the default error text unless a text has been passed
			    (let ((error-text (if (null ,text)
						  e
						  ,text)))
			      (error-message error-text 
					     :title (if (null ,title) (format nil "~a" (type-of e)) ,title)
					     :button :retry-cancel))))))
		  ;; Check the state \ button pressed
		  (cond ((string= 'retry ,result)
			 (,attempt))
			((string= 'cancel ,result)
			 'cancel)
			(t (values 'done ,return-data))))))
       (multiple-value-bind (,result ,return-data)
	   (,attempt)
	 ;;if we canceled, run ,cancel function     
	 (cond ((string= 'cancel ,result)
		,cancel)
	       ((string= 'done ,result)
		,return-data))))))


(defun release-error (error-hook)
  "Changes the error hook, defaults into an error message that'll quit the program"
  (setf *debugger-hook*
	(cond ((equal error-hook t)
	       
	       (lambda (error hook)
		 (declare (ignore hook))
		 (ftw:message-box :text (format nil "~a" error)
				  :button :ok :caption (format nil "~a" (type-of error))
				  :button :ok
				  :icon :error)
		 (quit);ccl:quit
		 ))
	      ((equal error-hook nil) *debugger-hook*)
	      (t error-hook))))

