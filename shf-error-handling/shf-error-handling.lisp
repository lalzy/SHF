;;;; sdl-helper-functions.lisp

(in-package #:shf-error-handling)

;; Make a dynamic try-retry error

(defparameter *quit-on-error* t)

(cffi:define-foreign-library win32
  (t (:default "User32")))

(cffi:use-foreign-library win32)

(cffi:defcfun (%messagebox% "MessageBoxW") :int
  (hwnd :pointer)
  (lpText :pointer)
  (lpCaption :pointer)
  (uType :uint32))

(defun quit ()
   #+:ccl (ccl:quit)
   #+:sbcl (sb-ext:exit))

(defun create-type (button icon right-leading right-to-left service-notification)
  (+
   (ecase button
     (:ok #x0)
     ((:ok-cancel :cancel) #x1)
    ; (:help #x4000) Doesn't work, probably need to create an win32 event
     ((:yes :no :yes-no) #x4)
     ((:yes-cancel :no-cancel :yes-no-cancel) #x3)
     ((:cancel-try-continue :try-continue :try) #x6)
     ((:abort :ignore :abort-retry-ignore) #x2)
     ((:retry :retry-cancel) #x5))
   (ecase icon
     (:none #x0)
     ((:exclamation :warning) #x30)
     ((:information :asterisk) #x40)
     ((:question :question-mark) #x20)
     ((:stop :error :hand) #x10))
   (if right-leading #x00080000 #x0)
   (if right-to-left #x00100000 #x0)
   (if service-notification #x00200000 #x0)))
   
  
(defun error-message (text &key (caption "Error!") (button :ok) (icon :error) right-to-left right-leading service-notification)
  "Opens up an windows error message box"
  (cffi:with-foreign-strings ((box-text text :encoding :utf-16) (caption-text caption :encoding :utf-16))
    (case
	(%messagebox% (cffi:null-pointer)
			box-text
			caption-text
			(create-type button icon right-to-left right-leading service-notification))
      (1 :ok)
      (2 :cancel)
      (3 :abort)
      (4 :retry)
      (5 :ignore)
      (6 :yes)
      (7 :no)
      (11 :continue)
      (10 :try))))

(defmacro try-retry (try-func &key (cancel-function '(quit))
				(text nil) (title nil))
  "A try-retry error system. Takes a function to attempt an operation on, 
  if an error occurs will throw an retry-cancel message box that'll attempt the same operation everytime retry is pressed
  Text is the error text, title is the message box title, cancel is what to do if it fails, default is exit"
  (alexandria:with-gensyms (return-data attempt-label result)
    `(labels ((,attempt-label () ; Used for recursion
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
					     :caption (if (null ,title) (format nil "~a" (type-of e)) ,title)
					     :button :retry-cancel))))))
		  
		  (case ,result
		    (:retry (,attempt-label))
		    (:cancel :cancel)
		    (t (values :done ,return-data))))))
       
       (multiple-value-bind (,result ,return-data)
	   (,attempt-label)
	 ;;if we canceled, run ,cancel function
	 (case ,result
	   (:cancel ,cancel-function)
	   (:done ,return-data))))))


(defun release-error (error-hook)
  "Changes the error hook, defaults into an error message that'll quit the program"
  (setf *debugger-hook*
	(cond ((equal error-hook t)
	       
	       (lambda (error hook)
		 (declare (ignore hook))
		 (error-message (format nil "~a" error)
				:caption (format nil "~a" (type-of error))
				:button :ok
				:icon :error)
		 (quit)
		 ))
	      ((equal error-hook nil) *debugger-hook*)
	      (t error-hook))))

