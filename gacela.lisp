;;; Gacela, a GNU Common Lisp extension for fast games development
;;; Copyright (C) 2009 by Javier Sancho Fernandez <jsf at jsancho dot org>
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


(in-package :gacela)

;;; Default values for Gacela
(defvar *width-screen* 640)
(defvar *height-screen* 480)
(defvar *bpp-screen* 32)
(defvar *title-screen* "Happy Hacking!!")
(defvar *frames-per-second* 20)
(defvar *transparent-color* '(:red 0 :green 0 :blue 0))
(defvar *background-color* '(:red 0 :green 0 :blue 0))

;;; SDL Initialization Subsystem
(let (initialized)

  (defun init-sdl ()
    (cond ((null initialized) (setq initialized (SDL_Init SDL_INIT_EVERYTHING)))
	  (t initialized)))

  (defun quit-sdl ()
    (setq initialized (SDL_Quit))))


;;; Video Subsystem
(defstruct surface address clip-w clip-h shape)

(let (screen flags)

  (defun init-video-mode (&key (width *width-screen*) (height *height-screen*) (bpp *bpp-screen*))
    (cond ((null screen)
	   (init-sdl)
	   (SDL_GL_SetAttribute SDL_GL_DOUBLEBUFFER 1)
	   (setq flags (+ SDL_OPENGL SDL_GL_DOUBLEBUFFER SDL_HWPALETTE SDL_RESIZABLE
			  (if (= (getf (SDL_GetVideoInfo) :hw_available) 0) SDL_SWSURFACE SDL_HWSURFACE)
			  (if (= (getf (SDL_GetVideoInfo) :blit_hw) 0) 0 SDL_HWACCEL)))
	   (setq screen (SDL_SetVideoMode width height bpp flags))
	   (init-GL)
	   (resize-screen-GL width height))
	  (t t)))

  (defun resize-screen (width height bpp)
    (setq screen (SDL_SetVideoMode width height bpp flags))
    (resize-screen-GL width height))

  (defun fill-screen (color)
    (init-video-mode)
    (fill-surface screen (getf color :red) (getf color :green) (getf color :blue)))

  (defun flip ()
    (cond ((null screen) nil)
	  (t (SDL_Flip screen))))

  (defun quit-video-mode ()
    (setq screen nil)))


(defun init-GL ()
  (2d-mode)
  (glShadeModel GL_SMOOTH)
  (glClearColor 0 0 0 0)
  (glClearDepth 1)
  (glDepthFunc GL_LEQUAL)
;  (glEnable GL_BLEND)
;  (glBlendFunc GL_SRC_ALPHA GL_ONE)
  (glHint GL_PERSPECTIVE_CORRECTION_HINT GL_NICEST)
  t)

(defun init-lighting ()
  (init-video-mode)
  (glEnable GL_LIGHTING))

(defun resize-screen-GL (width height)
  (let ((ratio (if (= height 0) width (/ width height))))
;    (glViewPort 0 0 width height)
    (glMatrixMode GL_PROJECTION)
    (glLoadIdentity)
    (let* ((w (/ width 2)) (-w (neg w)) (h (/ height 2)) (-h (neg h)))
      (glOrtho -w w -h h 0 1))
;    (gluPerspective 45 ratio 0.1 100)
    (glMatrixMode GL_MODELVIEW)
    (glLoadIdentity)
    t))

(let ((current-color '(1 1 1 1)))
  (defun get-current-color ()
    current-color)

  (defun set-current-color (red green blue &optional (alpha 1))
    (setq current-color (list red green blue alpha))
    (glColor4f red green blue alpha)))

(defun load-image (image-file &key (transparent-color nil))
  (init-video-mode)
  (let ((loaded-image (IMG_Load image-file)))
    (cond ((= loaded-image 0) nil)
	  (t (let ((optimized-image (SDL_DisplayFormat loaded-image)))
	       (SDL_FreeSurface loaded-image)
	       (cond ((= optimized-image 0) nil)
		     ((null transparent-color) optimized-image)
		     (t (SDL_SetColorKey optimized-image
					 SDL_SRCCOLORKEY
					 (SDL_MapRGB (surface-format optimized-image)
						     (car transparent-color)
						     (cadr transparent-color)
						     (caddr transparent-color)))
			optimized-image)))))))

(defun load-image2 (image-file &key (transparent-color nil))
  (let ((address-image (load-image image-file :transparent-color transparent-color)))
    (list
     (lambda (x y) (print-surface x y address-image))
     (lambda () (SDL_FreeSurface address-image)))))

(defun clean-screen ()
  (fill-screen *background-color*))

(defun refresh-screen ()
  (clean-screen)
  (funcall-procs #'print-mob)
  (flip))


;;; Audio Subsystem
(let ((audio nil))

  (defun init-audio ()
    (cond ((null audio) (progn (init-sdl) (setq audio (Mix_OpenAudio 22050 2 4096))))
	  (t audio)))

  (defun quit-audio ()
    (setq audio (Mix_CloseAudio))))


;;; Resources Manager
(defstruct resource plist constructor destructor time)

(defun make-resource-texture (&key filename min-filter mag-filter)
  `(:type texture :filename ,filename :min-filter ,min-filter :mag-filter ,mag-filter))

(defun make-resource-font (&key filename encoding)
  `(:type font :filename ,filename :enconding ,encoding))

(defmacro get-rtime (key)
  `(resource-time (gethash ,key resources-table)))

(defmacro get-rplist (key)
  `(resource-plist (gethash ,key resources-table)))

(defmacro get-rconstructor (key)
  `(resource-constructor (gethash ,key resources-table)))

(defmacro get-rdestructor (key)
  `(resource-destructor (gethash ,key resources-table)))

(let ((resources-table (make-hash-table :test 'equal))
      (expiration-time 50000))

  (defun set-expiration-time (new-time)
    (setq expiration-time new-time))

  (defun set-resource (key plist constructor destructor &key static)
    (expire-resources)
    (setf (gethash key resources-table)
	  (make-resource :plist plist
			 :constructor constructor
			 :destructor destructor
			 :time (if static t (SDL_GetTicks)))))

  (defun get-resource (key)
    (cond ((null (gethash key resources-table)) nil)
	  (t (let ((time (get-rtime key)))
	       (cond ((null time) (funcall (get-rconstructor key)))
		     ((numberp time) (setf (get-rtime key) (SDL_GetTicks))))
	       (get-rplist key)))))

  (defun free-resource (key)
    (funcall (get-rdestructor key))
    (setf (get-rtime key) nil))

  (defun expire-resource (key &optional (now (SDL_GetTicks)))
    (let ((time (get-rtime key)))
      (cond ((and (numberp time) (> (- now time) expiration-time)) (free-resource key)))))

  (defun expire-resources ()
    (maphash (lambda (key res) (expire-resource key)) resources-table))

  (defun free-all-resources ()
    (maphash (lambda (key res) (free-resource key)) resources-table)))


;;; Connection with the GUI
(let (socket)
  (defun connect-to-gui ()
    (setq socket (si::socket 1984 :host "localhost")))

  (defun eval-from-gui ()
    (cond ((and socket (listen socket)) (eval (read socket))))))


;;; GaCeLa Functions
(let (commands)
  (defun prog-command (command)
    (setq commands (cons command commands)))

  (defun run-commands ()
    (cond (commands
	   (let (running)
	     (setq running commands)
	     (setq commands nil)
	     (labels ((run-com (comlst)
			       (cond (comlst (run-com (cdr comlst))
					     (eval (read-from-string (concatenate 'string "(progn " (car comlst) ")")))))))
		     (run-com running)))))))

(let (time (time-per-frame (/ 1000.0 *frames-per-second*)))
  (defun set-frames-per-second (fps)
    (setq time-per-frame (/ 1000.0 fps)))

  (defun init-frame-time ()
    (setq time (SDL_GetTicks)))

  (defun delay-frame ()
    (let ((frame-time (- (SDL_GetTicks) time)))
      (cond ((< frame-time time-per-frame)
	     (SDL_Delay (- time-per-frame frame-time)))))))
      

(defmacro run-game (title &body code)
  `(progn
     (init-video-mode)
     (SDL_WM_SetCaption ,title "")
     (init-frame-time)
     (process-events)
     (do () ((quit?))
	 (glClear (+ GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
	 (glLoadIdentity)
	 ,@code
	 (SDL_GL_SwapBuffers)
	 (delay-frame)
	 (init-frame-time)
	 (process-events)
	 (setq running nil))))

(defun quit-game ()
  (free-all-resources)
;  (quit-audio)
  (quit-video-mode)
;  (quit-all-procs)
;  (clear-events)
;  (quit-events)
  (quit-sdl))
