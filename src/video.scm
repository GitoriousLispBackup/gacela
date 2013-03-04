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
;  #:use-module (gacela gl)
  #:use-module (figl gl)
  #:use-module (figl glu)
  #:use-module (gacela ftgl)
  #:use-module (gacela math)
  #:use-module (gacela utils)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 receive)
  #:export (init-video
	    get-screen-height
	    get-screen-width
	    get-screen-bpp
	    set-screen-bpp!
	    resize-screen
	    quit-video
	    clear-screen
	    flip-screen
	    set-screen-title!
	    get-screen-title
	    set-2d-mode
	    set-3d-mode
	    3d-mode?
	    get-frames-per-second
	    set-frames-per-second!
	    get-fullscreen
	    set-fullscreen!
	    init-frame-time
	    get-frame-time
	    delay-frame
	    get-current-color
	    set-current-color
	    with-color
	    progn-textures
	    draw
	    load-texture
	    load-texture-without-cache
	    get-texture-properties
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
	    load-font-without-texture
	    render-text)
  #:re-export (with-gl-push-matrix))



;;; Screen

(define screen #f)
(define flags 0)

(define* (init-video width height bpp #:key (mode '2d) (title "") (fps 20) (fullscreen 'off))
  (SDL_Init SDL_INIT_VIDEO)
  (let ((info (SDL_GetVideoInfo)))
    (SDL_GL_SetAttribute SDL_GL_DOUBLEBUFFER 1)
    (set! flags (+ SDL_OPENGL SDL_GL_DOUBLEBUFFER SDL_HWPALETTE SDL_RESIZABLE
		   (if (= (assoc-ref info 'hw_available) 0) SDL_SWSURFACE SDL_HWSURFACE)
		   (if (= (assoc-ref info 'blit_hw) 0) 0 SDL_HWACCEL)
		   (if (eq? fullscreen 'on) SDL_FULLSCREEN 0)))
    (set! screen (SDL_SetVideoMode width height bpp flags))
    (set-screen-title! title)
    (set-frames-per-second! fps)
    (set-fullscreen! fullscreen #f)
    (init-gl)
    (if (eq? mode '3d) (set-3d-mode) (set-2d-mode))))

(define (get-screen-height)
  (surface-h screen))

(define (get-screen-width)
  (surface-w screen))

(define (get-screen-bpp)
  (* (surface-format-BytesPerPixel screen) 8))

(define (set-screen-bpp! bpp)
  (cond (screen
	 (set! screen (SDL_SetVideoMode (get-screen-width) (get-screen-height) (get-screen-bpp) flags)))))

(define (resize-screen width height)
  (cond (screen
	 (set! screen (SDL_SetVideoMode width height (get-screen-bpp) flags))
	 (resize-screen-GL width height))))

(define (quit-video)
  (cond (screen
	 (SDL_FreeSurface screen)
	 (set! screen #f)
	 (SDL_Quit))))

(define (clear-screen)
  (gl-clear (clear-buffer-mask color-buffer depth-buffer)))

(define (flip-screen)
  (SDL_GL_SwapBuffers))


(define screen-title "")

(define (set-screen-title! title)
  (set! screen-title title)
  (SDL_WM_SetCaption title ""))

(define (get-screen-title)
  screen-title)


(define mode '2d)

(define (set-2d-mode)
  (set! mode '2d)
  (gl-disable (enable-cap depth-test))
  (resize-screen-GL (get-screen-width) (get-screen-height)))

(define (set-3d-mode)
  (set! mode '3d)
  (set-gl-clear-depth 1)
  (gl-enable (enable-cap depth-test))
  (set-gl-depth-function (depth-function lequal))
  (resize-screen-GL (get-screen-width) (get-screen-height)))

(define (3d-mode?)
  (eq? mode '3d))


(define fullscreen 'off)

(define* (set-fullscreen! fs #:optional (toggle #t))
  (cond ((or (and (eq? fullscreen 'on) (eq? fs 'off))
	     (and (eq? fullscreen 'off) (eq? fs 'on)))
	 (set! fullscreen fs)
	 (cond (toggle
		(SDL_WM_ToggleFullScreen screen))))))

(define (get-fullscreen)
  fullscreen)


(define (init-gl)
  (set-gl-shade-model (shading-model smooth))
  (set-gl-clear-color 0 0 0 0)
  (gl-enable (enable-cap blend))
  (set-gl-blend-function (blending-factor-dest src-alpha) (blending-factor-dest one-minus-src-alpha))
  (glHint (hint-target perspective-correction-hint) (hint-mode nicest)))

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

(define time 0)
(define frames-per-second 20)
(define time-per-frame 50)   ;in ms

(define (get-frames-per-second)
  frames-per-second)

(define (set-frames-per-second! fps)
  (set! frames-per-second fps)
  (set! time-per-frame (/ 1000.0 fps)))

(define (init-frame-time)
  (set! time (SDL_GetTicks)))

(define (get-frame-time)
  time)

(define (delay-frame)
  (let ((frame-time (- (SDL_GetTicks) time)))
    (cond ((< frame-time time-per-frame)
	   (SDL_Delay (- time-per-frame frame-time))))))


;;; Drawing

(define current-color '(1 1 1 1))

(define (get-current-color)
  current-color)

(define* (set-current-color red green blue #:optional (alpha 1))
  (set! current-color (list red green blue alpha))
  (glColor4f red green blue alpha))

(define-macro (with-color color . code)
  `(cond (,color
	  (let ((original-color (get-current-color))
		(result #f))
	    (apply set-current-color ,color)
	    (set! result (begin ,@code))
	    (apply set-current-color original-color)
	    result))
	 (else (begin ,@code))))

(define-macro (progn-textures . code)
  `(let ((result #f))
     (gl-enable GL_TEXTURE_2D)
     (set! result (begin ,@code))
     (gl-disable GL_TEXTURE_2D)
     result))

(define (draw . vertexes)
  (begin-draw (length vertexes))
  (draw-vertexes vertexes)
  (glEnd))

(define (begin-draw number-of-points)
  (cond ((= number-of-points 2) (glBegin GL_LINES))
	((= number-of-points 3) (glBegin GL_TRIANGLES))
	((= number-of-points 4) (glBegin GL_QUADS))
	((> number-of-points 4) (glBegin GL_POLYGON))))

(define (draw-vertexes vertexes)
  (cond ((not (null? vertexes))
	 (draw-vertex (car vertexes))
	 (draw-vertexes (cdr vertexes)))))

(define* (draw-vertex vertex #:key texture-coord)
  (cond (texture-coord (apply glTexCoord2f texture-coord)))
  (apply simple-draw-vertex vertex))

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

(define* (load-texture-without-cache filename #:key (min-filter GL_LINEAR) (mag-filter GL_LINEAR))
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

(define load-texture (use-cache-with load-texture-without-cache))

(define (get-texture-properties texture)
  `((width . ,(texture-w texture)) (height . ,(texture-h texture))))

(define* (draw-texture texture #:key (zoom 1) (sprite '((0 0) (1 1))))
  (cond (texture
	 (let ((width (texture-w texture))
	       (height (texture-h texture)))
	   (draw-rectangle (* zoom width (- (caadr sprite) (caar sprite)))
			   (* zoom height (- (cadadr sprite) (cadar sprite)))
			   #:texture texture
			   #:texture-coord sprite)))))

(define* (draw-line length)
  (let ((l (/ length 2)))
    (draw (list 0 l) (list 0 (- l)))))

(define (draw-circle radius)
  (glBegin GL_POLYGON)
  (do ((i 0 (1+ i)))
      ((>= i 360))
    (let ((a (degrees-to-radians i)))
      (draw-vertex (list (* radius (cos a)) (* radius (sin a))))))
  (glEnd))

(define* (draw-quad v1 v2 v3 v4 #:key texture (texture-coord '((0 0) (1 1))))
  (cond (texture
	 (progn-textures
	  (glBindTexture GL_TEXTURE_2D texture)
	  (begin-draw 4)
	  (draw-vertex v1 #:texture-coord (car texture-coord))
	  (draw-vertex v2 #:texture-coord (list (caadr texture-coord) (cadar texture-coord)))
	  (draw-vertex v3 #:texture-coord (cadr texture-coord))
	  (draw-vertex v4 #:texture-coord (list (caar texture-coord) (cadadr texture-coord)))
	  (glEnd)))
	(else
	 (draw v1 v2 v3 v4))))

(define* (draw-rectangle width height #:key texture texture-coord)
  (let ((w (/ width 2)) (h (/ height 2)))
    (draw-quad (list (- w) h 0)
	       (list w h 0)
	       (list w (- h) 0)
	       (list (- w) (- h) 0)
	       #:texture texture
	       #:texture-coord texture-coord)))

(define* (draw-square size #:key texture)
  (draw-rectangle size size #:texture texture))

(define* (draw-cube #:key (size 1)
		   texture texture-1 texture-2 texture-3 texture-4 texture-5 texture-6
		   color-1 color-2 color-3 color-4 color-5 color-6)
  (let ((-size (- size)))
    (progn-textures
     (glNormal3f 0 0 1)
     (with-color color-1 (draw-quad (list -size size size) (list size size size) (list size -size size) (list -size -size size) #:texture (or texture-1 texture)))
     (glNormal3f 0 0 -1)
     (with-color color-2 (draw-quad (list -size -size -size) (list size -size -size) (list size size -size) (list -size size -size) #:texture (or texture-2 texture)))
     (glNormal3f 0 1 0)
     (with-color color-3 (draw-quad (list size size size) (list -size size size) (list -size size -size) (list size size -size) #:texture (or texture-3 texture)))
     (glNormal3f 0 -1 0)
     (with-color color-4 (draw-quad (list -size -size size) (list size -size size) (list size -size -size) (list -size -size -size) #:texture (or texture-4 texture)))
     (glNormal3f 1 0 0)
     (with-color color-5 (draw-quad (list size -size -size) (list size -size size) (list size size size) (list size size -size) #:texture (or texture-5 texture)))
     (glNormal3f -1 0 0)
     (with-color color-6 (draw-quad (list -size -size size) (list -size -size -size) (list -size size -size) (list -size size size) #:texture (or texture-6 texture))))))

(define* (gtranslate x y #:optional (z 0))
  (glTranslatef x y z))

(define (grotate . rot)
  (cond ((3d-mode?)
	 (apply 3d-rotate rot))
	(else
	 (2d-rotate (car (last-pair rot))))))

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

;; (define* (add-light #:key light position ambient (id GL_LIGHT1) (turn-on t))
;;   (init-lighting)
;;   (and light (glLightfv id GL_DIFFUSE (car light) (cadr light) (caddr light) (cadddr light)))
;;   (and light position (glLightfv GL_POSITION (car position) (cadr position) (caddr position) (cadddr position)))
;;   (and ambient (glLightfv id GL_AMBIENT (car ambient) (cadr ambient) (caddr ambient) (cadddr ambient)))
;;   (and turn-on (gl-enable id))
;;   id)


;;; Camera

(define camera-eye '(0 0 0))
(define camera-center '(0 0 -100))
(define camera-up '(0 1 0))

(define* (set-camera #:key eye center up)
  (cond (eye (set! camera-eye eye)))
  (cond (center (set! camera-center center)))
  (cond (up (set! camera-up up))))

(define (camera-look)
  (apply gluLookAt (append camera-eye camera-center camera-up)))


;;; Text and fonts

(define* (load-font-without-cache font-file #:key (size 40) (encoding ft_encoding_unicode))
  (let ((font (ftglCreateTextureFont font-file size)))
    (ftglSetFontFaceSize font size 72)
    (ftglSetFontCharMap font encoding)
    font))

(define load-font (use-cache-with load-font-without-cache))

(define* (render-text text font #:key (size #f))
  (cond (size
	 (cond ((not (= (ftglGetFontFaceSize font) size))
		(ftglSetFontFaceSize font size 72))))
	((not (= (ftglGetFontFaceSize font) (font-size font)))
	 (ftglSetFontFaceSize font (font-size font) 72)))
  (ftglRenderFont font text FTGL_RENDER_ALL))


;;; Meshes

(define mesh-type
  (make-record-type "mesh" 
 		    '(draw translate turn rotate color inner-properties inner-property properties properties-set! property property-set!)
 		    (lambda (record port)
 		      (format port "#<mesh: ~a" (mesh-inner-property record 'type))
 		      (for-each (lambda (x) (format port " ~a" x))
 				(mesh-properties record))
 		      (display ">" port))))

(define mesh? (record-predicate mesh-type))

(define* (make-mesh type proc)
  (apply
   (record-constructor mesh-type)
   (let ((px 0) (py 0) (pz 0)
 	 (ax 0) (ay 0) (az 0)
 	 (rx 0) (ry 0) (rz 0)
	 (color #f)
 	 (properties '()))
     (let ((inner-properties
 	    (lambda ()
 	      `((type . ,type) (color . ,color)
		(x . ,px) (y . ,py) (z . ,pz)
		(ax . ,ax) (ay . ,ay) (az . ,az)
		(rx . ,rx) (ry . ,ry) (rz . ,rz)))))
       (list
 	(lambda ()
 	  "draw"
 	  (glmatrix-block
 	   (grotate ax ay az)
 	   (gtranslate px py pz)
 	   (grotate rx ry rz)
	   (with-color color (proc properties))))
 	(lambda (x y z)
 	  "translate"
 	  (set! px (+ px x))
 	  (set! py (+ py y))
 	  (set! pz (+ pz z)))
 	(lambda (x y z)
 	  "turn"
 	  (set! ax (+ ax x))
 	  (set! ay (+ ay y))
 	  (set! az (+ az z)))
 	(lambda (x y z)
 	  "rotate"
 	  (set! rx (+ rx x))
 	  (set! ry (+ ry y))
 	  (set! rz (+ rz z)))
	(lambda (c)
	  "color"
	  (set! color c))
 	(lambda ()
 	  "inner-properties"
 	  (inner-properties))
 	(lambda (prop-name)
 	  "inner-property"
 	  (assoc-ref (inner-properties) prop-name))
 	(lambda ()
 	  "properties"
 	  properties)
 	(lambda (new-properties)
 	  "properties-set!"
 	  (set! properties new-properties))
 	(lambda (prop-name)
 	  "property"
 	  (assoc-ref properties prop-name))
 	(lambda (prop-name value)
 	  "property-set!"
 	  (set! properties (assoc-set! properties prop-name value))))))))

(define (mesh-draw mesh)
  (((record-accessor mesh-type 'draw) mesh)))

(define (mesh-inner-properties mesh)
  (((record-accessor mesh-type 'inner-properties) mesh)))

(define (mesh-inner-property mesh prop-name)
  (((record-accessor mesh-type 'inner-property) mesh) prop-name))

(define (mesh-properties mesh)
  (((record-accessor mesh-type 'properties) mesh)))

(define (mesh-properties-set! mesh new-properties)
  (((record-accessor mesh-type 'properties-set!) mesh) new-properties))

(define (mesh-property mesh prop-name)
  (((record-accessor mesh-type 'property) mesh) prop-name))

(define (mesh-property-set! mesh prop-name value)
  (((record-accessor mesh-type 'property-set!) mesh) prop-name value))

(define* (translate mesh x y #:optional (z 0))
  (((record-accessor mesh-type 'translate) mesh) x y z)
  mesh)

(define (turn mesh . params)
  (apply ((record-accessor mesh-type 'turn) mesh)
 	 (if (>= (length params) 3)
 	     params
 	     (list 0 0 (car params))))
  mesh)

(define (rotate mesh . params)
  (apply ((record-accessor mesh-type 'rotate) mesh)
 	 (if (>= (length params) 3)
 	     params
 	     (list 0 0 (car params))))
  mesh)

(define (color mesh c)
  (((record-accessor mesh-type 'color) mesh) c)
  mesh)


;;; Advanced meshes

(define (mesh-join . meshes)
  (make-mesh
   'joined-meshes
   (lambda (props)
     (for-each (lambda (m) (glmatrix-block (mesh-draw m))) meshes))))


;;; Primitives

(define-macro (primitive header . body)
  (let* ((type (car header))
	 (args (cdr header))
	 (list-args (names-arguments args)))
    `(lambda* ,args
       (let ((m (make-mesh
		 ',type
		 (lambda (props)
		   (apply (lambda* ,(cons #:key list-args) ,@body)
			  (list
			   ,@(let get-params ((l list-args))
			       (cond ((null? l) '())
				     (else
				      (cons (symbol->keyword (car l))
					    (cons `(assoc-ref props ',(car l))
						  (get-params (cdr l)))))))))))))
	 (mesh-properties-set! m (list ,@(map (lambda (a) `(cons ',a ,a)) list-args)))
	 m))))

(define-macro (define-primitive header . body)
  `(define ,(car header) (primitive ,header ,@body)))


;;; Primitives definition

(define-primitive (square size #:key texture)
  (draw-square size #:texture texture))

(define-primitive (rectangle width height #:key texture texture-coord)
  (draw-rectangle width height #:texture texture #:texture-coord texture-coord))

(define-primitive (circle radius)
  (draw-circle radius))

(define-primitive (picture filename #:key (min-filter GL_LINEAR) (mag-filter GL_LINEAR) (zoom 1) (sprite '((0 0) (1 1))))
  (draw-texture (load-texture filename #:min-filter min-filter #:mag-filter mag-filter) #:zoom zoom #:sprite sprite))


(module-map (lambda (sym var)
	      (if (not (eq? sym '%module-public-interface))
		  (module-export! (current-module) (list sym))))
	    (current-module))