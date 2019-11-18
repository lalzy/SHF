;;;; package.lisp
(defpackage #:sdl-helper-functions
  (:nicknames  #:shf)
  (:use #:cl #:cffi #:iterate)
  (:export

   ;; Helper functions
   #:create-exe

   ;; Main engine stuff
   :*height*
   :*width*
   :*debug*
   :*debug-hitbox-draw*
   :*key-pressed-code*
   :*key-pressed-state*
   #:main-loop

   #:new-main
   

   ;; Keyboard
   #:is-keys
   #:check-key
   #:get-pressed-key

   ;; Mouse
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

   ;; Generic drawing things

   ;; Generic Accessors or class parameter editors
   #:rect
   #:circle
   #:get-surface
   #:x #:y #:w #:h #:r
   #:incf-x #:incf-y
   #:set-x #:set-y
   #:change-surface 

   
   ;; Text
   :*font-path*
   :*fonts*
   #:line-wrapping
   #:get-font
   #:change-default-font
   #:draw-text
   #:draw-text-with-line-wrap
   #:draw-words ; delete?
   #:draw-texts
   #:draw-debug-text
   #:show-menu-text ; ?
   #:draw-text-with-lines
   
   ;; Text Field
   #:text-field
   #:is-active?
   #:text-field-background
   #:create-text-field
   #:change-text-field-state
   #:draw-text-field
   #:draw-text-on-text-field
   #:draw-text-field-with-text

   #:input-text-to-field
   #:get-text
   #:get-text-y
   #:get-text-font
   #:get-line-amount
   #:text-scrolling

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
   #:scrolling
   #:get-scrollbox-hitbox ; Might change to get-hitboxes(which sprite will use)

   
   ;; Sound
   :*list-of-sounds*
   :*sound-frequency*
   :*sound-path*
   #:init-sounds
   #:play-sound
   #:set-volume

   ;; Music
   :*list-of-music*
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

   ;; Sprite class and it's Accessors
   #:sprite-class
   #:get-sprite
   #:get-sprite-animations
   #:get-sprite-hitboxes ; Change to a generic get-hitboxes
   
   ;; Hitbox Class and it's Accessors
   #:hitbox-class
   #:get-hitbox-name
   #:get-hitbox ; ? If surface, change to get-surface
   #:get-alpha ; remove, no need to store alpha values
   
 ;  #:get-hitbox-color
   #:get-box-color
   #:get-collision-hitbox-name
   #:get-hitbox-rel-pos
   ))

