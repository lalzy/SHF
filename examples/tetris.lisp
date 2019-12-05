;;;; package.lisp

(in-package #:shf-examples)

#||

22h 12w - 

5x5px grid\square.


#    #  ##  ##
##  #           ##  ##  #    #
## ###   ####    #  #   #    #

#           #
#    ##   ###   ##
###   #     #     #

#     ##   #     ##
##   ##    ##   ##
#         # 

##   #      #   ###
#    ###    #     #
#          ##     


#
#
####   #
#


Create square sprites, 5x5 pixles
Create shapes with these squares:


Create grids of 5x5 squares around a 12x22 grids square for the map
Represent map as an 2 dimentional array of '(12 22)

#xxx
##xx
x#xx
xxxx

||#

(defparameter *lshape* #2a((1 0 0 0)
			   (1 0 0 0)
			   (1 1 0 0)
			   (0 0 0 0)))

(defparameter *tshape* #2a((nil 1 nil nil)
			   (1 1 1 nil)
			   (nil nil nil nil)
			   (nil nil nil nil)))

(defparameter *map-x* 12)
(defparameter *map-y* 22)
(defparameter *map* (make-array (list *map-x* *map-y*)))

(defparameter *box-size* 20)

(defun draw-box (x y)
  (sdl:draw-box-* x y *box-size* *box-size* :color (shf:get-color green))
  (sdl:draw-rectangle-* x y (1+ *box-size*) (1+ *box-size*) :color (shf:get-color black))
  )

(defun draw-map-box (x y)
  (sdl:draw-box-* x y *box-size* *box-size* :color (shf:get-color white))
  (sdl:draw-rectangle-* x y (1+ *box-size*) (1+ *box-size*) :color (shf:get-color black))
  )

(defun calc-cord (num)
  (* *box-size* (1+ num)))

(defun tetris ()
  (let ()
    (shf:main-loop
     (:width 400 :height 500 :font-path "c:/te/")
     (:init (setf (aref *map* 5 6) 1
		  (aref *map* 6 5) 1
		  (aref *map* 6 6) 1
		  (aref *map* 6 7) 1))
     
     (:main
  (sdl:draw-rectangle-* 20 20 (* 20 12) (* 20 22) :color (shf:get-color white))
      (loop for x to (1- *map-x*) do
		 (loop for y to (1- *map-y*) do
		      
		    ;  (draw-map-box (calc-cord x) (calc-cord y))
		      ;(sdl:draw-box-* (calc-cord x) (calc-cord y) *box-size* *box-size* :color (shf:get-color white))
				    (when (= (aref *map* x y) 1)
				      (draw-box (calc-cord x) (calc-cord y)))))))))
