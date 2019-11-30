(in-package :sdl-helper-functions)

;;; Create images \ Sprite sheets
(defun make-image (image &key (path "") color-key-pos)
  "Loads up an image from path and returns it as a surface"
  (shf-error:try-retry
   (sdl:load-and-convert-image (merge-pathnames path image) :color-key-at color-key-pos)
   :title "Cannot load image error"))

(defun sub-image (image cell color-key-pos)
  "Subsects image cells into seperate surfaces"
  (let* ((x (elt cell 0))
	 (y (elt cell 1))
	 (w (elt cell 2))
	 (h (elt cell 3))
	 (new-surface (sdl:create-surface w h :color-key-at color-key-pos)))
  (setf (sdl:cells image) (sdl:rectangle :x x :y y :h h :w w))
  (sdl:draw-surface image :surface new-surface)
  new-surface))


(defun generate-sheet-cells (cell-width sprite-width &key (cell-height cell-width) (sprite-height sprite-width))
  "Generate the sprite-cell list"
  (iter (for y from 0 to cell-height by sprite-height)
	(appending (iter (for x from 0 to cell-width by sprite-width)
			 (collect (list x y sprite-width sprite-height))))))
#||
  (loop for y from 0 to cell-height by sprite-height append
       (loop for x from 0 to cell-width by sprite-width collect (list x y sprite-width sprite-height))))
||#
(defun make-sprite-sheet (image cells &key (color-key-pos))
  "Returns a list of sprite surfaces cut by cells(Sequence) ex:
   ((0 0 32 32) (0 32 32 32) (0 64 32 32))"
  (loop for cell in cells collect (sub-image image cell color-key-pos)))
