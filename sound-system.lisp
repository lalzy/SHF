;;; Make song and music list into arrays

(in-package #:sdl-helper-functions)

;(defparameter *list-of-sounds* nil) ; AList of sound samples
(defparameter *list-of-sounds* (make-hash-table)) ; AList of sound samples
(defparameter *list-of-music* nil) ; AList of sound samples
;(defparameter *list-of-music* (make-hash-table)) ; AList of sound samples
(defparameter *music-keys* nil)
(defparameter *sound-frequency* 64000) ; The sound frequency
(defparameter *sound-path* "") ; Path where sound-files are st4ored
(defparameter *music-path* "") ; Path where sound-files are stored
(defparameter *current-song-index* 0) ; Current song playing(as index number)
;(defparameter *current-song* nil) ; Current song playing(as index number)

(defmethod remove-extention (sound)
  "Removes extention from sound-name"
  ;; Converts to a string if it isn't already
  
  (unless (stringp sound)
    (setf sound (format nil "~a" sound)))
  
  ;; Looks for . from the end to the beginning, then returns a subseq from beginning to right before the .
  (let* ((index (length sound)))
    (iter (for x :in-sequence sound :downto 0)
	  (decf index)
	  (when (equalp x #\.)
	    (return)))

    (if (<= index 0)
	sound
	(subseq sound 0 index))))

(defun remove-path (file path)
  "removes the path from a file-name"
  (unless (stringp file)
    (setf file (format nil "~a" file)))

  (unless (stringp path)
    (setf path (format nil "~a" file)))
  (let* ((start (search path file))
	 (pos (when (numberp start) (+ (length path) start))))
    (if pos
	(subseq file pos)
	file)))

(defun get-filename (file path)
  "Get the clean file name"
  (let ((name (remove-path file path)))
    (setf name (remove-extention name))
    name))

(defun create-key-name (sound path)
  "Create a key identifier for hash-table \ alist using the file-name"
  (read-from-string (format nil ":~a" (get-filename sound path))))

(defun create-file-name (file-name extention)
  (format nil "~a.~a" file-name extention))

(defun load-audio-files (path file-names extention type)
  "Loads up audio files from [path], type is either music or sample."
  ;; load up all files in path unless we pass the files we want
  (unless file-names
    (setf file-names (remove-if-not (lambda (file) (search extention (namestring file)))
				    (uiop:directory-files path))))
 ; (shf-error:try-retry
   (dolist (sound file-names)
     
     (format t "sound = ~a, ext = ~a~%" sound extention)
     ;; Sound and Music is handled differently
     (cond ((string= type 'music)
	    (push (list (create-key-name sound path)
			(sdl-mixer:load-music (merge-pathnames
					       (if (search extention (namestring sound))
						   sound
						   (create-file-name sound extention))
					       path)))  *list-of-music*))
	   
	   ((string= type 'sample)
	    (let ((key (create-key-name sound path)))
	      (setf (gethash key *list-of-sounds*)
		    (sdl-mixer:load-sample (merge-pathnames
					   (if (search extention (namestring sound))
						sound
						(create-file-name sound extention))
						path)))))))
 ) ; :title "Cannot load file error"))

(defun init-sounds (path &key file-names (extention "wav"))
  "Takes a file [path] to load audio files from. 
Keyed optionals:
   File-names - a list of files, if not passed will load all files of [extention] in [path]
   extention - file [extention] of the files, without the '.'"
  (load-audio-files path file-names extention 'sample))

(defun init-music (path &key file-names (extention "ogg"))
  "Takes a file [path] to load audio files from. 
Keyed optionals:
   File-names - a list of files, if not passed will load all files of [extention] in [path]
   extention - file [extention] of the files, without the '.'"
  (load-audio-files path file-names extention 'music))

(defun rotate-playlist ()
  "Plays all songs in the list of music in sequense"
  (if (= *current-song-index* (1- (length *list-of-music*)))
      (setf *current-song-index* 0)
      (incf *current-song-index*)))

(defun random-music ()
  "Plays a random song from the list of music"
  (when *list-of-music*
    (let ((new-song (random (length *list-of-music*))))
      (if (= new-song *current-song-index*)
	  (random-music)
	  (setf *current-song-index* new-song)))))

(defun play-current-song ()
  "Start playing *current-song* from the list of music"
  (play-song (car (elt *list-of-music* *current-song-index*))))

(defun get-current-song-name ()
  "Get the current selected\playing song's file name"
  (when *list-of-music*
    (car (elt *list-of-music* *current-song-index*))))

(defun play-song (song)
  "Takes the song name in form of keyword symbol, plays the song"
  (when *list-of-music*
  (sdl-mixer:play-music (second (assoc song *list-of-music*)));(get-sound song *list-of-music*))
  (setf *current-song-index* (position song *list-of-music* :key #'car))))

(defun stop-music ()
  "Stop music playing"
  (sdl-mixer:halt-music))

(defun get-music-volume ()
  "Get the current music volume"
  (sdl-mixer:music-volume))

(defun set-volume (amount &aux (volume (sdl-mixer:channel-volume -1)))
  "Change the current volume by [amount]."
  (incf volume amount)
  (when (< volume 0) (setf volume 0))
  (setf (sdl-mixer:channel-volume nil) volume)
  (format t "sdl mixer = ~a~%" (sdl-mixer:channel-volume -1)))

(defun set-music-volume ( amount &aux (volume (sdl-mixer:music-volume)))
  "Change the music volume by [amount]"
  (incf volume amount)
  (when (< volume 0) (setf volume 0))
  (when (> volume sdl-mixer:+max-volume+) (setf volume sdl-mixer:+max-volume+))
  (setf (sdl-mixer:music-volume) volume))

(defun play-sound (sound &optional (looping nil))
  "Takes the sound name in form of keyword symbol, plays the sound"
  (sdl-mixer:play-sample (gethash sound *list-of-sounds*) :loop looping))
 ; (sdl-mixer:play-sample (get-sound sound *list-of-sounds*) :loop looping))

(defmacro music-stopped-form (&body body)
  "What to do once music stops playing"
  `(unless (sdl-mixer:music-playing-p)
     ,@body))
