(in-package #:sdl-helper-functions)

(defmacro push-last (element list )
  "pushes element to last position in list(destructive)"
  `(if ,list
       (push ,element (cdr (last ,list)))
       (push ,element ,list)))

(defun last-index (sequence)
  (1- (length sequence)))

(defun create-adj-string (&optional text)
  "Create an adjustable string"
  (make-array (if text (length text) 0) :element-type 'character :adjustable t
	      :fill-pointer (if text (length text) 0)
	      :initial-contents (if text text nil)))


(defmacro strarg (string &rest args)
  "create a string with arguments"
  `(format nil ,string ,@args))


(defmacro sbreak (string &rest args)
  `(break (strarg ,string ,@args)))


(defun get-option-from-alist (option options &optional nil-override)
  (let ((result (cadr (assoc option options))))
    (if (and (not result) nil-override)
	nil-override
	result)))
