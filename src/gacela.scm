;;; Gacela, a GNU Guile extension for fast games development
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


;;; Default values for Gacela

(define *title* "Gacela")
(define *width-screen* 640)
(define *height-screen* 480)
(define *bpp-screen* 32)
(define *frames-per-second* 20)
(define *mode* '2d)


;;; SDL Initialization Subsystem

(define init-sdl #f)
(define sdl-on? #f)
(define quit-sdl #f)

(let ((initialized #f))
  (set! init-sdl
	(lambda ()
	  (cond ((not initialized) (SDL_Init SDL_INIT_EVERYTHING) (set! initialized #t))
		(else initialized))))

  (set! sdl-on?
	(lambda ()
	  (if initialized #t #f)))

  (set! quit-sdl
	(lambda ()
	  (SDL_Quit)
	  (set! initialized #f))))


;;; Video Subsystem

(define init-video-mode #f)
(define video-mode-on? #f)
(define resize-screen #f)
(define quit-video-mode #f)

(let ((screen #f) (flags 0))
  (set! init-video-mode
	(lambda ()
	  (cond ((not screen)
		 (init-sdl)
		 (let* ((props (get-game-properties))
			(width (assoc-ref props 'width)) (height (assoc-ref props 'height))
			(bpp (assoc-ref props 'bpp)) (title (assoc-ref props 'title))
			(mode (assoc-ref props 'mode))
			(info (SDL_GetVideoInfo)))
		   (SDL_GL_SetAttribute SDL_GL_DOUBLEBUFFER 1)
		   (set! flags (+ SDL_OPENGL SDL_GL_DOUBLEBUFFER SDL_HWPALETTE SDL_RESIZABLE
				  (if (= (assoc-ref info 'hw_available) 0) SDL_SWSURFACE SDL_HWSURFACE)
				  (if (= (assoc-ref info 'blit_hw) 0) 0 SDL_HWACCEL)))
		   (set! screen (SDL_SetVideoMode width height bpp flags))
		   (SDL_WM_SetCaption title "")
		   (init-gl)
		   (if (eq? mode '3d) (set-3d-mode) (set-2d-mode))))
		(else #t))))

  (set! video-mode-on?
	(lambda () (if screen #t #f)))

  (set! resize-screen
	(lambda* (width height #:optional (bpp current-bpp))
	  (cond (screen (set! screen (SDL_SetVideoMode width height bpp flags))
			(resize-screen-GL width height)))))

  (set! quit-video-mode
	(lambda ()
	  (SDL_FreeSurface screen)
	  (set! screen #f))))

(define (set-2d-mode)
  (cond ((not (3d-mode?))
	 (init-video-mode)
	 (glDisable GL_DEPTH_TEST)
	 (apply-mode-change))))

(define (set-3d-mode)
  (cond ((3d-mode?)
	 (init-video-mode)
	 (glClearDepth 1)
	 (glEnable GL_DEPTH_TEST)
	 (glDepthFunc GL_LEQUAL)
	 (apply-mode-change))))

(define (apply-mode-change)
  (let* ((props (get-game-properties))
	 (width (assoc-ref props 'width)) (height (assoc-ref props 'height)))
    (resize-screen-GL width height)))

(define (3d-mode?)
  (eq? (assoc-ref (get-game-properties) 'mode) '3d))

(define (init-gl)
  (glShadeModel GL_SMOOTH)
  (glClearColor 0 0 0 0)
;  (glClearDepth 1)
;  (glDepthFunc GL_LEQUAL)
  (glEnable GL_BLEND)
;  (glBlendFunc GL_SRC_ALPHA GL_ONE)
  (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
  (glHint GL_PERSPECTIVE_CORRECTION_HINT GL_NICEST)
  #t)

(define (init-lighting)
  (init-video-mode)
  (glEnable GL_LIGHTING))

(define (resize-screen-GL width height)
  (glViewport 0 0 width height)
  (glMatrixMode GL_PROJECTION)
  (glLoadIdentity)
  (cond ((3d-mode?) (let ((ratio (if (= height 0) width (/ width height))))
		      (gluPerspective 45 ratio 0.1 100))) ;0.1
	(else (let* ((w (/ width 2)) (h (/ height 2)))
		(glOrtho (- w) w (- h) h 0 1))))
  (glMatrixMode GL_MODELVIEW)
  (glLoadIdentity)
  #t)

(define get-current-color #f)
(define set-current-color #f)

(let ((current-color '(1 1 1 1)))
  (set! get-current-color
	(lambda ()
	  current-color))

  (set! set-current-color
	(lambda* (red green blue #:optional (alpha 1))
	  (set! current-color (list red green blue alpha))
	  (glColor4f red green blue alpha))))


;;; Audio Subsystem

(define init-audio #f)
(define quit-audio #f)

(let ((audio #f))
  (set! init-audio
	(lambda ()
	  (cond ((not audio) (begin (init-sdl) (set! audio (Mix_OpenAudio 22050 MIX_DEFAULT_FORMAT 2 4096))))
		(else audio))))

  (set! quit-audio
	(lambda ()
	  (Mix_CloseAudio)
	  (set! audio #f))))


;;; Resources Cache

(define resources-cache (make-weak-value-hash-table))

(define get-resource-from-cache #f)
(define insert-resource-into-cache #f)

(let ()
  (set! get-resource-from-cache
	(lambda (key)
	  (hash-ref resources-cache key)))

  (set! insert-resource-into-cache
	(lambda (key res)
	  (hash-set! resources-cache key res))))

;;; GaCeLa Functions

(define set-frames-per-second #f)
(define init-frame-time #f)
(define delay-frame #f)

(let ((time 0) (time-per-frame (/ 1000.0 *frames-per-second*)))
  (set! set-frames-per-second
	(lambda (fps)
	  (set! time-per-frame (/ 1000.0 fps))))

  (set! init-frame-time
	(lambda ()
	  (set! time (SDL_GetTicks))))

  (set! delay-frame
	(lambda ()
	  (let ((frame-time (- (SDL_GetTicks) time)))
	    (cond ((< frame-time time-per-frame)
		   (SDL_Delay (- time-per-frame frame-time))))))))


(define set-game-properties! #f)
(define get-game-properties #f)

(let ((ptitle *title*) (pwidth *width-screen*) (pheight *height-screen*) (pbpp *bpp-screen*) (pfps *frames-per-second*) (pmode *mode*))
  (set! set-game-properties!
	(lambda* (#:key title width height bpp fps mode)
;	  (init-video-mode)
	  (if title
	      (begin
		(set! ptitle title)
		(if (video-mode-on?) (SDL_WM_SetCaption title ""))))
	  (if (or width height bpp)
	      (begin
		(if width (set! pwidth width))
		(if height (set! pheight height))
		(if bpp (set! pbpp bpp))
		(if (video-mode-on?) (resize-screen pwidth pheight pbpp))))
	  (if fps
	      (begin
		(set! pfps fps)
		(set-frames-per-second fps)))
	  (if mode
	      (begin
		(set! pmode mode)
		(if (video-mode-on?)
		    (if (eq? mode '3d) (set-3d-mode) (set-2d-mode)))))
	  (get-game-properties)))

  (set! get-game-properties
	(lambda ()
	  `((title . ,ptitle) (width . ,pwidth) (height . ,pheight) (bpp . ,pbpp) (fps . ,pfps) (mode . ,pmode)))))


(define-macro (run-game . code)
  `(let ((game-function ,(if (null? code)
			     `(lambda () #f)
			     `(lambda () ,@code))))
     (init-video-mode)
     (set-game-code game-function)
     (cond ((not (game-running?))
	    (game-loop)))))

(define game-loop #f)
(define game-running? #f)
(define set-game-code #f)

(let ((running #f) (game-code #f))
  (set! game-loop
	(lambda ()
	  (refresh-active-mobs)
	  (set! running #t)
	  (quit! #f)
	  (do () ((quit?))
	    (init-frame-time)
	    (check-connections)
	    (eval-from-clients)
	    (process-events)
	    (cond ((not (quit?))
		   (cond ((video-mode-on?)
			  (glClear (+ GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
			  (to-origin)))
		   (refresh-active-mobs)
		   (if (procedure? game-code)
		       (catch #t
			      (lambda () (game-code))
			      (lambda (key . args) #f)))
		   (cond ((video-mode-on?)
			  (run-mobs)
			  (SDL_GL_SwapBuffers)))
		   (delay-frame))))
	  (set! running #f)))

  (set! game-running?
	(lambda ()
	  running))

  (set! set-game-code
	(lambda (game-function)
	  (set! game-code game-function))))
