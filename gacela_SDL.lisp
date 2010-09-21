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


(eval-when (compile load eval)
	   (when (not (find-package 'gacela)) (make-package 'gacela :nicknames '(gg) :use '(lisp)))
	   (in-package 'gacela :nicknames '(gg) :use '(lisp)))


(defmacro mapcconst (type c-type name)
  (let ((c-header (concatenate 'string c-type " gacela_" name " (void)"))
	(c-body (concatenate 'string "return " name ";"))
	(c-name (concatenate 'string "gacela_" name))
	(lisp-name (intern (string-upcase name))))
    `(progn
       (defcfun ,c-header 0 ,c-body)
       (defentry ,lisp-name () (,type ,c-name))
       (eval-when (load) (defconstant ,lisp-name (,lisp-name))))))

(clines "#include <SDL/SDL.h>")
(clines "#include <SDL/SDL_image.h>")
(clines "#include <SDL/SDL_mixer.h>")

;;; SDL constants as functions
(mapcconst int "int" "SDL_INIT_TIMER")
(mapcconst int "int" "SDL_INIT_AUDIO")
(mapcconst int "int" "SDL_INIT_VIDEO")
(mapcconst int "int" "SDL_INIT_CDROM")
(mapcconst int "int" "SDL_INIT_JOYSTICK")
(mapcconst int "int" "SDL_INIT_NOPARACHUTE")
(mapcconst int "int" "SDL_INIT_EVENTTHREAD")
(mapcconst int "int" "SDL_INIT_EVERYTHING")

(mapcconst int "int" "SDL_SWSURFACE")
(mapcconst int "int" "SDL_HWSURFACE")
(mapcconst int "int" "SDL_ASYNCBLIT")

(mapcconst int "int" "SDL_ANYFORMAT")
(mapcconst int "int" "SDL_HWPALETTE")
(mapcconst int "int" "SDL_DOUBLEBUF")
(mapcconst int "int" "SDL_FULLSCREEN")
(mapcconst int "int" "SDL_OPENGL")
(mapcconst int "int" "SDL_OPENGLBLIT")
(mapcconst int "int" "SDL_RESIZABLE")
(mapcconst int "int" "SDL_NOFRAME")

(mapcconst int "int" "SDL_HWACCEL")
(mapcconst int "int" "SDL_SRCCOLORKEY")

(mapcconst int "int" "SDL_GL_DOUBLEBUFFER")

(mapcconst int "int" "SDL_DEFAULT_REPEAT_DELAY")
(mapcconst int "int" "SDL_DEFAULT_REPEAT_INTERVAL")

(mapcconst int "int" "SDL_LIL_ENDIAN")
(mapcconst int "int" "SDL_BIG_ENDIAN")

;;; SDL Functions
(defcfun "int gacela_SDL_Init (int flags)" 0
  "return SDL_Init (flags);")

(defcfun "void gacela_SDL_Quit (void)" 0
  "SDL_Quit ();")

(defcfun "int gacela_SDL_SetVideoMode (int width, int height, int bpp, int flags)" 0
  "close(2);"
  "return SDL_SetVideoMode (width, height, bpp, flags);")

(defcfun "void gacela_SDL_WM_SetCaption (char *title, char *icon)" 0
  "SDL_WM_SetCaption (title, icon);")

(defcfun "int gacela_SDL_Flip (int screen)" 0
  "return SDL_Flip (screen);")

(defcfun "void gacela_SDL_FreeSurface (int surface)" 0
  "SDL_FreeSurface (surface);")

(defcfun "void gacela_SDL_Delay (int ms)" 0
  "SDL_Delay (ms);")

(defcfun "int gacela_SDL_GetTicks (void)" 0
  "return SDL_GetTicks ();")

(defcfun "int gacela_SDL_DisplayFormat (int surface)" 0
  "return SDL_DisplayFormat (surface);")

(defcfun "int gacela_SDL_MapRGB (int format, int r, int g, int b)" 0
  "return SDL_MapRGB (format, r, g, b);")

(defcfun "int gacela_SDL_SetColorKey (int surface, int flag, int key)" 0
  "return SDL_SetColorKey (surface, flag, key);")

(defcfun "int gacela_SDL_LoadBMP (char *file)" 0
  "return SDL_LoadBMP (file);")

(defcfun "int gacela_IMG_Load (char *filename)" 0
  "return IMG_Load (filename);")

(defcfun "static object gacela_SDL_GetVideoInfo (void)" 0
  "const SDL_VideoInfo *info;"
  "object vi, label;"
  "info = SDL_GetVideoInfo ();"
  ('nil vi)
  ((cons (int info->blit_hw) vi) vi) (':blit_hw label) ((cons label vi) vi)
  ((cons (int info->hw_available) vi) vi) (':hw_available label) ((cons label vi) vi)
  "return vi;")

(defcfun "int gacela_SDL_GL_SetAttribute (int attr, int value)" 0
  "return SDL_GL_SetAttribute (attr, value);")

(defcfun "static object gacela_SDL_PollEvent (void)" 0
  "SDL_Event sdl_event;"
  "object event, label;"
  ('nil event)
  "if (SDL_PollEvent (&sdl_event)) {"
  "  switch (sdl_event.type) {"
  "    case SDL_KEYDOWN:"
  "    case SDL_KEYUP:"
  ((cons (int sdl_event.key.keysym.sym) event) event) (':key.keysym.sym label) ((cons label event) event)
  "      break;"
  "  }"
  ((cons (int sdl_event.type) event) event) (':type label) ((cons label event) event)
  "}"
  "return event;")

(defcfun "void gacela_SDL_GL_SwapBuffers (void)" 0
  "SDL_GL_SwapBuffers ();")

(defcfun "int gacela_SDL_EnableKeyRepeat (int delay, int interval)" 0
  "return SDL_EnableKeyRepeat (delay, interval);")

(defcfun "int gacela_SDL_ByteOrder (void)" 0
  "return SDL_BYTEORDER;")

(defcfun "int gacela_zoomSurface (int src, double zoomx, double zoomy, int smooth)" 0
  "return zoomSurface (src, zoomx, zoomy, smooth);")

(defentry SDL_Init (int) (int "gacela_SDL_Init"))
(defentry SDL_Quit () (void "gacela_SDL_Quit"))
(defentry SDL_SetVideoMode (int int int int) (int "gacela_SDL_SetVideoMode"))
(defentry SDL_WM_SetCaption (string string) (void "gacela_SDL_WM_SetCaption"))
(defentry SDL_Flip (int) (int "gacela_SDL_Flip"))
(defentry SDL_FreeSurface (int) (void "gacela_SDL_FreeSurface"))
(defentry SDL_Delay (int) (void "gacela_SDL_Delay"))
(defentry SDL_GetTicks () (int "gacela_SDL_GetTicks"))
(defentry SDL_DisplayFormat (int) (int "gacela_SDL_DisplayFormat"))
;(defentry SDL_SurfaceFormat (int) (int "gacela_SDL_SurfaceFormat"))
(defentry SDL_MapRGB (int int int int) (int "gacela_SDL_MapRGB"))
(defentry SDL_SetColorKey (int int int) (int "gacela_SDL_SetColorKey"))
;(defentry SDL_BlitSurface (int int int int) (void "gacela_SDL_BlitSurface"))
;(defentry SDL_Rect (int int int int) (int "gacela_SDL_Rect"))
(defentry SDL_LoadBMP (string) (int "gacela_SDL_LoadBMP"))
(defentry IMG_Load (string) (int "gacela_IMG_Load"))
(defentry SDL_GetVideoInfo () (object "gacela_SDL_GetVideoInfo"))
(defentry SDL_GL_SetAttribute (int int) (int "gacela_SDL_GL_SetAttribute"))
(defentry SDL_PollEvent () (object "gacela_SDL_PollEvent"))
;(defentry TTF_Init () (int "gacela_TTF_Init"))
;(defentry TTF_OpenFont (string int) (int "gacela_TTF_OpenFont"))
;(defentry TTF_CloseFont (int) (void "gacela_TTF_CloseFont"))
;(defentry TTF_Quit () (void "gacela_TTF_Quit"))
;(defentry Mix_OpenAudio (int int int) (int "gacela_Mix_OpenAudio"))
;(defentry Mix_LoadMUS (string) (int "gacela_Mix_LoadMUS"))
;(defentry Mix_LoadWAV (string) (int "gacela_Mix_LoadWAV"))
;(defentry Mix_PlayChannel (int int int) (int "gacela_Mix_PlayChannel"))
;(defentry Mix_PlayMusic (int int) (int "gacela_Mix_PlayMusic"))
;(defentry Mix_PlayingMusic () (int "gacela_Mix_PlayingMusic"))
;(defentry Mix_PausedMusic () (int "gacela_Mix_PausedMusic"))
;(defentry Mix_PauseMusic () (void "gacela_Mix_PauseMusic"))
;(defentry Mix_ResumeMusic () (void "gacela_Mix_ResumeMusic"))
;(defentry Mix_HaltMusic () (int "gacela_Mix_HaltMusic"))
;(defentry Mix_FreeMusic (int) (void "gacela_Mix_FreeMusic"))
;(defentry Mix_FreeChunk (int) (void "gacela_Mix_FreeChunk"))
;(defentry Mix_CloseAudio () (void "gacela_Mix_CloseAudio"))
;(defentry free (int) (void "gacela_free"))
(defentry SDL_GL_SwapBuffers () (void "gacela_SDL_GL_SwapBuffers"))
(defentry SDL_EnableKeyRepeat (int int) (int "gacela_SDL_EnableKeyRepeat"))
(defentry SDL_ByteOrder () (int "gacela_SDL_ByteOrder"))
(defentry zoomSurface (int double double int) (int "gacela_zoomSurface"))

;;; C-Gacela Functions
(defcfun "int gacela_surface_format (int surface)" 0
  "const SDL_Surface *s = surface;"
  "return s->format;")

(defcfun "int gacela_surface_w (int surface)" 0
  "const SDL_Surface *s = surface;"
  "return s->w;")

(defcfun "int gacela_surface_h (int surface)" 0
  "const SDL_Surface *s = surface;"
  "return s->h;")

(defcfun "int gacela_surface_pixels (int surface)" 0
  "const SDL_Surface *s = surface;"
  "return s->pixels;")

(defcfun "int gacela_surface_format_BytesPerPixel (int surface)" 0
  "const SDL_Surface *s = surface;"
  "return s->format->BytesPerPixel;")

;(defentry apply-surface2 (int int int int int int int int int) (void "apply_surface"))
;(defentry render-text2 (int string int int int) (int "render_text"))
;(defentry box-collision (int int int int int int) (int "box_collision"))
;(defentry create-SDL_Surface (int int int int int int) (int "create_SDL_Surface"))
;(defentry copy-SDL_Surface (int) (int "copy_SDL_Surface"))
(defentry surface-format (int) (int "gacela_surface_format"))
(defentry surface-w (int) (int "gacela_surface_w"))
(defentry surface-h (int) (int "gacela_surface_h"))
(defentry surface-pixels (int) (int "gacela_surface_pixels"))
(defentry surface-format-BytesPerPixel (int) (int "gacela_surface_format_BytesPerPixel"))
