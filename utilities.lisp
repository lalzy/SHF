(in-package #:sdl-helper-functions)

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
