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


(define-module (gacela gacela)
  #:use-module (gacela events)
  #:use-module (gacela video)
  #:use-module (gacela audio)
  #:use-module (ice-9 optargs)
  #:export ())


;;; Default values for Gacela

(define *title* "Gacela")
(define *width-screen* 640)
(define *height-screen* 480)
(define *bpp-screen* 32)
(define *frames-per-second* 20)
(define *mode* '2d)


;;; Resources Cache

(define resources-cache (make-weak-value-hash-table))

(define from-cache #f)
(define into-cache #f)

(let ()
  (set! from-cache
	(lambda (key)
	  (hash-ref resources-cache key)))

  (set! into-cache
	(lambda (key res)
	  (hash-set! resources-cache key res))))

(define-macro (use-cache-with-procedure proc-name)
  `(begin
     (define ,(string->symbol (string-concatenate (list (symbol->string proc-name) "-without-cache"))) ,proc-name)))


;;; GaCeLa Functions

(define set-frames-per-second #f)
(define init-frame-time #f)
(define get-frame-time #f)
(define delay-frame #f)

(let ((time 0) (time-per-frame (/ 1000.0 *frames-per-second*)))
  (set! set-frames-per-second
	(lambda (fps)
	  (set! time-per-frame (/ 1000.0 fps))))

  (set! init-frame-time
	(lambda ()
	  (set! time (SDL_GetTicks))))

  (set! get-frame-time
	(lambda ()
	  time))

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
