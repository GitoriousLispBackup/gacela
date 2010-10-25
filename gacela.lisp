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


(eval-when (compile load) (make-package 'gacela :nicknames '(gg) :use '(lisp)))

(eval-when (compile load eval)
	   (when (not (find-package 'gacela)) (make-package 'gacela :nicknames '(gg) :use '(lisp)))
	   (in-package 'gacela :nicknames '(gg) :use '(lisp)))


;;; Default values for Gacela
(defvar *width-screen* 640)
(defvar *height-screen* 480)
(defvar *bpp-screen* 32)
(defvar *frames-per-second* 20)

;;; SDL Initialization Subsystem
(let (initialized)

  (defun init-sdl ()
    (cond ((null initialized) (setq initialized (SDL_Init SDL_INIT_EVERYTHING)))
	  (t initialized)))

  (defun quit-sdl ()
    (setq initialized (SDL_Quit))))


;;; Video Subsystem
(let (screen flags (current-width *width-screen*) (current-height *height-screen*) current-bpp)

  (defun init-video-mode (&key (width current-width) (height current-height) (bpp *bpp-screen*))
    (cond ((null screen)
	   (init-sdl)
	   (SDL_GL_SetAttribute SDL_GL_DOUBLEBUFFER 1)
	   (setq flags (+ SDL_OPENGL SDL_GL_DOUBLEBUFFER SDL_HWPALETTE SDL_RESIZABLE
			  (if (= (getf (SDL_GetVideoInfo) :hw_available) 0) SDL_SWSURFACE SDL_HWSURFACE)
			  (if (= (getf (SDL_GetVideoInfo) :blit_hw) 0) 0 SDL_HWACCEL)))
	   (setq screen (SDL_SetVideoMode width height bpp flags))
	   (init-GL)
	   (resize-screen-GL width height)
	   (setq current-width width current-height height current-bpp bpp))
	  (t t)))

  (defun resize-screen (width height &optional (bpp current-bpp))
    (cond (screen (setq screen (SDL_SetVideoMode width height bpp flags))
		  (resize-screen-GL width height)))
    (setq current-width width current-height height))

  (defun apply-mode-change ()
    (resize-screen-GL current-width current-height))

  (defun quit-video-mode ()
    (setq screen nil)))

(defun set-2d-mode ()
  (cond ((not (3d-mode?))
	 (init-video-mode)
	 (glDisable GL_DEPTH_TEST)
	 (apply-mode-change))))

(defun set-3d-mode ()
  (cond ((3d-mode?)
	 (init-video-mode)
	 (glClearDepth 1)
	 (glEnable GL_DEPTH_TEST)
	 (glDepthFunc GL_LEQUAL)
	 (apply-mode-change))))

(defun 3d-mode? ()
  (eq (getf (get-game-properties) :mode) '3d))

(defun init-GL ()
  (glShadeModel GL_SMOOTH)
  (glClearColor 0 0 0 0)
;  (glClearDepth 1)
;  (glDepthFunc GL_LEQUAL)
;  (glEnable GL_BLEND)
;  (glBlendFunc GL_SRC_ALPHA GL_ONE)
  (glHint GL_PERSPECTIVE_CORRECTION_HINT GL_NICEST)
  t)

(defun init-lighting ()
  (init-video-mode)
  (glEnable GL_LIGHTING))

(defun resize-screen-GL (width height)
  (glViewPort 0 0 width height)
  (glMatrixMode GL_PROJECTION)
  (glLoadIdentity)
  (cond ((3d-mode?) (let ((ratio (if (= height 0) width (/ width height))))
		      (gluPerspective 45 ratio 0.1 100))) ;0.1
	(t (let* ((w (/ width 2)) (-w (neg w)) (h (/ height 2)) (-h (neg h)))
	     (glOrtho -w w -h h 0 1))))
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


;;; Audio Subsystem
(let ((audio nil))

  (defun init-audio ()
    (cond ((null audio) (progn (init-sdl) (setq audio (Mix_OpenAudio 22050 MIX_DEFAULT_FORMAT 2 4096))))
	  (t audio)))

  (defun quit-audio ()
    (setq audio (Mix_CloseAudio))))


;;; Resources Manager
(defstruct resource plist constructor destructor time)

(defun make-resource-texture (&key filename min-filter mag-filter)
  `(:type texture :filename ,filename :min-filter ,min-filter :mag-filter ,mag-filter))

(defun make-resource-font (&key filename encoding)
  `(:type font :filename ,filename :enconding ,encoding))

(defun make-resource-sound (&key filename)
  `(:type sound :filename ,filename))

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


;;; Connection with Gacela Clients
(let (server-socket clients)
  (defun start-server (port)
    (cond ((null server-socket) (setq server-socket (si::socket port :server #'check-connections)))))

  (defun check-connections ()
    (cond ((and server-socket (listen server-socket)) (setq clients (cons (si::accept server-socket) clients)))))

  (defun eval-from-clients ()
    (labels ((eval-clients (cli-socks)
			   (cond (cli-socks
				  (let ((cli (car cli-socks)))
				    (cond ((si::listen cli)
					   (secure-block cli (eval (read cli)))
					   (si::close cli)
					   (eval-clients (cdr cli-socks)))
					  (t
					   (cons cli (eval-clients (cdr cli-socks))))))))))
	    (setq clients (eval-clients clients))))

  (defun stop-server ()
    (cond (server-socket (si::close server-socket) (setq server-socket nil)))
    (cond (clients
	   (labels ((close-clients (cli-socks)
				   (si::close (car cli-socks))
				   (close-clients (cdr cli-socks))))
		   (close-clients clients))
	   (setq clients nil)))))


;;; GaCeLa Functions
(let (time (time-per-frame (/ 1000.0 *frames-per-second*)))
  (defun set-frames-per-second (fps)
    (setq time-per-frame (/ 1000.0 fps)))

  (defun init-frame-time ()
    (setq time (SDL_GetTicks)))

  (defun delay-frame ()
    (let ((frame-time (- (SDL_GetTicks) time)))
      (cond ((< frame-time time-per-frame)
	     (SDL_Delay (- time-per-frame frame-time)))))))


(let ((ptitle "") (pwidth *width-screen*) (pheight *height-screen*) (pbpp *bpp-screen*) (pfps *frames-per-second*) (pmode '2d))
  (defun set-game-properties (&key title width height bpp fps mode)
    (init-video-mode)
    (when title (progn (setq ptitle title) (SDL_WM_SetCaption title "")))
    (when (or width height bpp)
      (progn
	(when width (setq pwidth width))
	(when height (setq pheight height))
	(when bpp (setq pbpp bpp))
	(resize-screen pwidth pheight pbpp)))
    (when fps (progn (setq pfps fps) (set-frames-per-second fps)))
    (when mode (progn (setq pmode mode) (if (eq mode '3d) (set-3d-mode) (set-2d-mode))))
    (get-game-properties))

  (defun get-game-properties ()
    (list :title ptitle :width pwidth :height pheight :bpp pbpp :fps pfps :mode pmode)))


(defmacro run-game (&body code)
  `(let ((game-function (lambda () ,@code)))
     (init-video-mode)
     (set-game-code game-function)
     (cond ((not (game-running?))
	    (init-frame-time)
	    (process-events)
	    (game-loop)))))

(let (running game-code)
  (defun game-loop ()
    (setq running t)
    (do () ((quit?))
	(glClear (+ GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
	(to-origin)
	(when (functionp game-code) (funcall game-code))
	(SDL_GL_SwapBuffers)
	(delay-frame)
	(init-frame-time)
	(check-connections)
	(eval-from-clients)
	(process-events))
    (setq running nil))

  (defun game-running? ()
    running)

  (defun set-game-code (game-function)
    (setq game-code game-function)))

(defun quit-game ()
  (free-all-resources)
  (quit-audio)
  (quit-video-mode)
  (quit-all-mobs)
;  (clear-events)
;  (quit-events)
  (quit-sdl))
