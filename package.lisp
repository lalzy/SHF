;;;; package.lisp
(defpackage #:sdl-helper-functions
  (:nicknames  #:shf)
  (:use #:cl #:cffi #:iterate)
  (:export

   #:create-exe
   :*list-of-music*
   ;; Main stuff
   :*height*
   :*width*
   :*debug*
   :*debug-hitbox-draw*
   :*key-pressed-code*
   :*key-pressed-state*
   #:main-loop
   #:is-keys
   #:check-key
   #:get-pressed-key
   
   #:*mouse-move-direction*
   #:*mouse-state*
   #:*Current-mouse-button*
   #:warp-mouse-at-*
   #:warp-mouse
   #:create-cursor
   
   :*cursor*

   ;; Menu \ States
   :*state*
   #:set-state
   #:check-state
   #:create-menu
   
   ;; Colors
   :*colors*
   #:get-rgb
   #:get-color
   #:add-color
   
   ;; Text
   :*font-path*
   :*fonts*
   #:draw-text
   #:draw-text-with-line-wrap
   #:draw-words
   #:draw-texts
   #:draw-debug-text
   #:show-menu-text
   #:change-default-font
   #:get-font

   #:change-surface-parameters
   
   #:draw-text-with-lines

   #:line-wrapping

   
   ;; Text Field
   #:text-field
   #:get-surface
   #:text-field-active?
   #:text-field-background
   #:create-text-field
   #:change-text-field-state
   #:draw-text-field
   #:draw-text-on-text-field
   #:draw-text-field-with-text


   ;; Scroll Box
   #:create-scroll-box
   #:calculate-scroll-box-height
   #:draw-scroll-box
   #:get-bar-color
   #:draw-scroll-bar
   #:create-scroll-bar
   #:get-scroll-box-hitbox
   #:get-scroll-box
   #:show-scroll-bar?

   #:get-scrollbox-hitbox

   
   ;; Sound
   :*list-of-sounds*
   :*sound-frequency*
   :*sound-path*
   #:init-sounds
   #:play-sound
   #:set-volume

   ;; Music
   :*music-path*
   :*current-song-index*
   :*current-song*
   #:init-music
   #:play-song
   #:play-current-song
   #:stop-music
   #:music-stopped-form
   #:random-music
   #:set-music-volume
   #:get-music-volume
   #:rotate-playlist
   #:get-current-song-name
   
   ;; Collision
   #:collision-range
   #:create-hitbox
   #:collision-check
   #:make-collision-circle
   #:mouse-collision-check
   #:mouse-text-collision
   #:edge-collision-check
   #:get-edge-dir

   
   ;; Sprite stuff
   #:move-sprite
   #:set-sprite-pos
   #:make-image
   #:make-sprite-sheet
   #:make-box-sprite
   #:make-circle-sprite
   #:draw-sprites 
   #:draw-hitboxes
   #:empty-sprite-group
   #:delete-from-sprite-group
   #:add-to-sprite-group
   #:Clone-sprite

   ;; Shapes and their accessors(and pos accessors)
   #:rect
   #:circle
   #:x #:y #:w #:h #:r
   #:incf-x #:incf-y
   #:set-x #:set-y

   #:change-surface
   
   ;; Sprite class and it's Accessors
   #:sprite-class
   #:get-sprite
   #:get-sprite-animations
   #:get-sprite-hitboxes
   
   ;; Hitbox Class and it's Accessors
   #:hitbox-class
   #:get-hitbox-name
   #:get-hitbox
   #:get-alpha
 ;  #:get-hitbox-color
   #:get-box-color
   #:get-collision-hitbox-name
   #:get-hitbox-rel-pos
   ))

