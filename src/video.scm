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


(define-module (gacela video)
  #:use-module (gacela sdl)
  #:use-module (gacela gl)
  #:use-module (gacela ftgl)
  #:use-module (gacela math)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 receive)
  #:export (init-video
	    get-screen-height
	    get-screen-width
	    get-screen-bpp
	    resize-screen
	    quit-video
	    clear-screen
	    flip-screen
	    set-2d-mode
	    set-3d-mode
	    3d-mode?
	    set-frames-per-second
	    init-frame-time
	    get-frame-time
	    delay-frame
	    get-current-color
	    set-current-color
	    with-color
	    progn-textures
	    draw
	    load-texture
	    draw-texture
	    draw-line
	    draw-quad
	    draw-rectangle
	    draw-square
	    draw-cube
	    translate
	    rotate
	    to-origin
	    add-light
	    set-camera
	    camera-look
	    load-font
	    render-text))


;;; Screen

(define init-video #f)
(define get-screen-height #f)
(define get-screen-width #f)
(define get-screen-bpp #f)
(define resize-screen #f)
(define quit-video #f)

(let ((screen #f) (flags 0))
  (set! init-video
	(lambda* (width height bpp #:key (mode '2d) (title ""))
	  (cond ((not screen)
		 (SDL_Init SDL_INIT_VIDEO)
		 (let ((info (SDL_GetVideoInfo)))
		   (SDL_GL_SetAttribute SDL_GL_DOUBLEBUFFER 1)
		   (set! flags (+ SDL_OPENGL SDL_GL_DOUBLEBUFFER SDL_HWPALETTE SDL_RESIZABLE
				  (if (= (assoc-ref info 'hw_available) 0) SDL_SWSURFACE SDL_HWSURFACE)
				  (if (= (assoc-ref info 'blit_hw) 0) 0 SDL_HWACCEL)))
		   (set! screen (SDL_SetVideoMode width height bpp flags))
		   (SDL_WM_SetCaption title "")
		   (init-gl)
		   (if (eq? mode '3d) (set-3d-mode) (set-2d-mode)))))))

  (set! get-screen-height
	(lambda ()
	  (surface-h screen)))

  (set! get-screen-width
	(lambda ()
	  (surface-w screen)))

  (set! get-screen-bpp
	(lambda ()
	  (surface-format-BytesPerPixel screen)))

  (set! resize-screen
	(lambda (width height)
	  (cond (screen
		 (set! screen (SDL_SetVideoMode width height (get-screen-bpp) flags))
		 (resize-screen-GL width height)))))

  (set! quit-video
	(lambda ()
	  (cond (screen
		 (SDL_FreeSurface screen)
		 (set! screen #f)
		 (SDL_Quit))))))

(define (clear-screen)
  (glClear (+ GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT)))

(define (flip-screen)
  (SDL_GL_SwapBuffers))


(define set-2d-mode #f)
(define set-3d-mode #f)
(define 3d-mode? #f)

(let ((mode '2d))
  (set! set-2d-mode
	(lambda ()
	  (set! mode '2d)
	  (glDisable GL_DEPTH_TEST)
	  (resize-screen-GL (get-screen-width) (get-screen-height))))

  (set! set-3d-mode
	(lambda ()
	  (set! mode '3d)
	  (glClearDepth 1)
	  (glEnable GL_DEPTH_TEST)
	  (glDepthFunc GL_LEQUAL)
	  (resize-screen-GL (get-screen-width) (get-screen-height))))

  (set! 3d-mode?
	(lambda ()
	  (eq? mode '3d))))


(define (init-gl)
  (glShadeModel GL_SMOOTH)
  (glClearColor 0 0 0 0)
  (glEnable GL_BLEND)
  (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
  (glHint GL_PERSPECTIVE_CORRECTION_HINT GL_NICEST))

(define (resize-screen-GL width height)
  (glViewport 0 0 width height)
  (glMatrixMode GL_PROJECTION)
  (glLoadIdentity)
  (cond ((3d-mode?)
	 (let ((ratio (if (= height 0) width (/ width height))))
	   (gluPerspective 45 ratio 0.1 100)))
	(else
	 (let* ((w (/ width 2)) (h (/ height 2)))
	   (glOrtho (- w) w (- h) h 0 1))))
  (glMatrixMode GL_MODELVIEW)
  (glLoadIdentity))


;;; Frames per second

(define set-frames-per-second #f)
(define init-frame-time #f)
(define get-frame-time #f)
(define delay-frame #f)

;; (let ((time 0) (time-per-frame (/ 1000.0 *frames-per-second*)))
;;   (set! set-frames-per-second
;; 	(lambda (fps)
;; 	  (set! time-per-frame (/ 1000.0 fps))))

;;   (set! init-frame-time
;; 	(lambda ()
;; 	  (set! time (SDL_GetTicks))))

;;   (set! get-frame-time
;; 	(lambda ()
;; 	  time))

;;   (set! delay-frame
;; 	(lambda ()
;; 	  (let ((frame-time (- (SDL_GetTicks) time)))
;; 	    (cond ((< frame-time time-per-frame)
;; 		   (SDL_Delay (- time-per-frame frame-time))))))))


;;; Drawing

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

(define-macro (with-color color . code)
  (cond (color
	 `(let ((original-color (get-current-color))
		(result #f))
	    (apply set-current-color ,color)
	    (set! result (begin ,@code))
	    (apply set-current-color original-color)
	    result))
	(else `(begin ,@code))))

(define-macro (progn-textures . code)
  `(let ((result #f))
     (glEnable GL_TEXTURE_2D)
     (set! result (begin ,@code))
     (glDisable GL_TEXTURE_2D)
     result))

(define (draw . vertexes)
  (begin-draw (length vertexes))
  (draw-vertexes vertexes)
  (glEnd))

(define (begin-draw number-of-points)
  (cond ((= number-of-points 2) (glBegin GL_LINES))
	((= number-of-points 3) (glBegin GL_TRIANGLES))
	((= number-of-points 4) (glBegin GL_QUADS))))

(define (draw-vertexes vertexes)
  (cond ((not (null? vertexes))
	 (draw-vertex (car vertexes))
	 (draw-vertexes (cdr vertexes)))))

(define* (draw-vertex vertex #:key texture-coord)
  (cond ((list? (car vertex))
	 (with-color (car vertex)
		     (apply simple-draw-vertex (cadr vertex))))
	(else
	 (cond (texture-coord (apply glTexCoord2f texture-coord)))
	 (apply simple-draw-vertex vertex))))

(define* (simple-draw-vertex x y #:optional (z 0))
  (cond ((3d-mode?) (glVertex3f x y z))
	(else (glVertex2f x y))))

(define (load-image filename)
  (let ((image (IMG_Load filename)))
    (cond (image
	   (SDL_DisplayFormatAlpha image)))))
  
(define (load-image-for-texture filename)
  (let ((image (load-image filename)))
    (cond (image
	   (let* ((width (surface-w image)) (height (surface-h image))
		  (power-2 (nearest-power-of-two (min width height)))
		  (resized-image #f))
	     (cond ((and (= width power-2) (= height power-2)) (values image width height))
		   (else (set! resized-image (resize-surface image power-2 power-2))
			 (if resized-image (values resized-image width height))))))
	  (else
	   (values #f 0 0)))))

(define (resize-surface surface width height)
  (let ((old-width (surface-w surface)) (old-height (surface-h surface)))
    (cond ((and (= width old-width) (= height old-height)) surface)
	  (else (let ((zoomx (/ (+ width 0.5) old-width)) (zoomy (/ (+ height 0.5) old-height)))
	       (zoomSurface surface zoomx zoomy 0))))))

(define* (load-texture filename #:key (min-filter GL_LINEAR) (mag-filter GL_LINEAR))
  (progn-textures
   (receive
    (image real-w real-h) (load-image-for-texture filename)
    (cond (image
	   (let ((width (surface-w image)) (height (surface-h image))
		 (byteorder (if (= SDL_BYTEORDER SDL_LIL_ENDIAN)
				(if (= (surface-format-BytesPerPixel image) 3) GL_BGR GL_BGRA)
				(if (= (surface-format-BytesPerPixel image) 3) GL_RGB GL_RGBA)))
		 (texture (car (glGenTextures 1))))

	     (glBindTexture GL_TEXTURE_2D texture)
	     (glTexImage2D GL_TEXTURE_2D 0 4 width height 0 byteorder GL_UNSIGNED_BYTE (surface-pixels image))
	     (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER min-filter)
	     (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER mag-filter)
	     (set-texture-size! texture real-w real-h)
	     texture))))))

(define* (draw-texture texture #:optional (zoom 1))
  (cond (texture
	 (let ((width (texture-w texture))
	       (height (texture-h texture)))
	   (draw-rectangle (* zoom width) (* zoom height) #:texture texture)))))

(define* (draw-line length #:optional color)
  (let ((l (/ length 2)))
    (cond (color
	   (with-color color (draw (list 0 l) (list 0 (- l)))))
	  (else
	   (draw (list 0 l) (list 0 (- l)))))))

(define* (draw-quad v1 v2 v3 v4 #:key texture color)
  (cond (texture
	 (progn-textures
	  (glBindTexture GL_TEXTURE_2D texture)
	  (begin-draw 4)
	  (draw-vertex v1 #:texture-coord '(0 0))
	  (draw-vertex v2 #:texture-coord '(1 0))
	  (draw-vertex v3 #:texture-coord '(1 1))
	  (draw-vertex v4 #:texture-coord '(0 1))
	  (glEnd)))
	(color
	 (with-color color (draw v1 v2 v3 v4)))
	(else
	 (draw v1 v2 v3 v4))))

(define* (draw-rectangle width height #:key texture color)
  (let ((w (/ width 2)) (h (/ height 2)))
    (draw-quad (list (- w) h 0)
	       (list w h 0)
	       (list w (- h) 0)
	       (list (- w) (- h) 0)
	       #:texture texture
	       #:color color)))

(define* (draw-square #:key (size 1) texture color)
  (draw-rectangle size size #:texture texture #:color color))

(define* (draw-cube #:key (size 1)
		   texture texture-1 texture-2 texture-3 texture-4 texture-5 texture-6
		   color color-1 color-2 color-3 color-4 color-5 color-6)
  (let ((-size (- size)))
    (progn-textures
     (glNormal3f 0 0 1)
     (draw-quad (list -size size size) (list size size size) (list size -size size) (list -size -size size) #:texture (or texture-1 texture) #:color (or color-1 color))
     (glNormal3f 0 0 -1)
     (draw-quad (list -size -size -size) (list size -size -size) (list size size -size) (list -size size -size) #:texture (or texture-2 texture) #:color (or color-2 color))
     (glNormal3f 0 1 0)
     (draw-quad (list size size size) (list -size size size) (list -size size -size) (list size size -size) #:texture (or texture-3 texture) #:color (or color-3 color))
     (glNormal3f 0 -1 0)
     (draw-quad (list -size -size size) (list size -size size) (list size -size -size) (list -size -size -size) :texture (or texture-4 texture) #:color (or color-4 color))
     (glNormal3f 1 0 0)
     (draw-quad (list size -size -size) (list size -size size) (list size size size) (list size size -size) :texture (or texture-5 texture) #:color (or color-5 color))
     (glNormal3f -1 0 0)
     (draw-quad (list -size -size size) (list -size -size -size) (list -size size -size) (list -size size size) :texture (or texture-6 texture) #:color (or color-6 color)))))

(define* (translate x y #:optional (z 0))
  (glTranslatef x y z))

(define* (rotate #:rest rot)
  (cond ((3d-mode?)
	 (apply 3d-rotate rot))
	(else
	 (apply 2d-rotate rot))))

(define (3d-rotate xrot yrot zrot)
  (glRotatef xrot 1 0 0)
  (glRotatef yrot 0 1 0)
  (glRotatef zrot 0 0 1))

(define (2d-rotate rot)
  (glRotatef rot 0 0 1))

(define (to-origin)
  (glLoadIdentity)
  (cond ((3d-mode?) (camera-look))))


;;; Lights

(define* (add-light #:key light position ambient (id GL_LIGHT1) (turn-on t))
  (init-lighting)
  (and light (glLightfv id GL_DIFFUSE (first light) (second light) (third light) (fourth light)))
  (and light position (glLightfv GL_POSITION (first position) (second position) (third position) (fourth position)))
  (and ambient (glLightfv id GL_AMBIENT (first ambient) (second ambient) (third ambient) (fourth ambient)))
  (and turn-on (glEnable id))
  id)


;;; Camera

(define set-camera #f)
(define camera-look #f)

(let ((camera-eye '(0 0 0)) (camera-center '(0 0 -100)) (camera-up '(0 1 0)))
  (set! set-camera
	(lambda* (#:key eye center up)
	  (cond (eye (set! camera-eye eye)))
	  (cond (center (set! camera-center center)))
	  (cond (up (set! camera-up up)))))

  (set! camera-look
	(lambda ()
	  (apply gluLookAt (append camera-eye camera-center camera-up)))))


;;; Text and fonts

(define* (load-font font-file #:key (size 40) (encoding ft_encoding_unicode))
  (let ((font (ftglCreateTextureFont font-file)))
    (ftglSetFontFaceSize font size 72)
    (ftglSetFontCharMap font encoding)
    font))

(define* (render-text text font #:key (size #f))
  (cond (size (ftglSetFontFaceSize font size 72)))
  (ftglRenderFont font text FTGL_RENDER_ALL))
