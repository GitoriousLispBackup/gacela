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

  (defun create-surface (width height &key (trans-color *transparent-color*))
    (init-video-mode)
    (let ((new-surface (make-surface
			:address (create-SDL_Surface
				  (surface-address screen)
				  width
				  height
				  (getf trans-color :red)
				  (getf trans-color :green)
				  (getf trans-color :blue)))))
      (set-resource 'image new-surface (gentemp))
      new-surface))

  (defun print-surface (x y surface)
    (apply-surface x y surface screen)
    surface)

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

(defmacro progn-textures (&body code)
  `(let (values)
     (init-video-mode)
     (glEnable GL_TEXTURE_2D)
     (setq values (multiple-value-list (progn ,@code)))
     (glDisable GL_TEXTURE_2D)
     (apply #'values values)))

(defun init-textures ()
  (init-video-mode)
  (glEnable GL_TEXTURE_2D))

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

(defun apply-surface (x y source destination)
  (let ((offset (SDL_Rect x y 0 0)))
    (SDL_BlitSurface source 0 destination offset)
    (free offset)
    destination))

(defun apply-surface-old (x y source destination &optional (clip nil))
  (cond ((null clip)
	 (apply-surface2 x y (surface-address source) (surface-address destination) 0 0 0 0 0))
	((integerp clip)
	 (apply-surface2 x y (surface-address source) (surface-address destination) 0 0
			 (surface-clip-w source) (surface-clip-h source) clip))
	(t
	 (apply-surface2 x y (surface-address source) (surface-address destination)
			 (first clip) (second clip) (third clip) (fourth clip) 0)))
  destination)


(defun print-image (x y image-file &optional (clip nil))
  (init-video-mode)
  (let ((image (load-image image-file)))
    (print-surface x y image clip)
    image))


(defun clean-screen ()
  (fill-screen *background-color*))

(defun refresh-screen ()
  (clean-screen)
  (funcall-procs #'print-mob)
  (flip))


;;; TTF Subsystem
(defstruct font address)

(let ((ttf nil))

  (defun init-ttf ()
    (cond ((null ttf) (progn (init-sdl) (setq ttf (TTF_Init))))
	  (t ttf)))

  (defun quit-ttf ()
    (setq ttf (TTF_Quit))))


(defun open-font (font-name tam)
  (init-ttf)
  (let ((font (get-resource 'font font-name tam)))
    (if (null font)
	(progn (setq font (make-font :address (TTF_OpenFont font-name tam)))
	       (set-resource 'font font font-name tam)))
    font))


(defun render-text (text-message
		    &key (color '(:red 255 :green 255 :blue 255))
		    (font-name "lazy.ttf") (tam 28))
  (init-ttf)
  (let ((message (get-resource 'text text-message color font-name tam)))
    (if (null message)
	(progn
	  (setq message
		(make-surface
		 :address (render-text2 (open-font font-name tam)
					text-message
					(getf color :red)
					(getf color :green)
					(getf color :blue))))
	  (set-resource 'text message text-message color font-name tam)))
    message))


(defun print-text (x y text-message
		     &key (color '(:red 255 :green 255 :blue 255))
		     (font-name "lazy.ttf") (tam 28))
  (init-video-mode)
  (init-ttf)
  (let ((message (render-text text-message :color color :font-name font-name :tam tam)))
    (print-surface x y message)
    message))


;;; Audio Subsystem
(let ((audio nil))

  (defun init-audio ()
    (cond ((null audio) (progn (init-sdl) (setq audio (Mix_OpenAudio 22050 2 4096))))
	  (t audio)))

  (defun quit-audio ()
    (setq audio (Mix_CloseAudio))))


;;; Resources Manager
(defstruct resource plist free-function time)

(defun make-texture (&key filename min-filter mag-filter)
  `(:type texture :filename ,filename :min-filter ,min-filter :mag-filter ,mag-filter))

(let ((resources-table (make-hash-table :test 'equal)))

  (defun set-resource (key plist free-function &key static)
    (setf (gethash key resources-table)
	  (make-resource :plist plist
			 :free-function free-function
			 :time (if static -1 (SDL_GetTicks)))))

  (defun get-resource (key)
    (let ((resource (gethash key resources-table)))
      (cond ((null resource) nil)
	    (t (cond ((/= (resource-time resource) -1)
		      (setf (resource-time resource) (SDL_GetTicks))
		      (setf (gethash key resources-table) resource)))
	       (resource-plist resource)))))

  (defun free-all-resources ()
    (maphash (lambda (key res) (funcall (resource-free-function res) (resource-address res)))
	     resources-table)
    (clrhash resources-table)))


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
;  (free-all-resources)
;  (quit-audio)
;  (quit-ttf)
  (quit-video-mode)
;  (quit-all-procs)
;  (clear-events)
;  (quit-events)
  (quit-sdl))
