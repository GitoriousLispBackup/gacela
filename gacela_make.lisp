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

(setq compiler::*cc* (concatenate 'string
				  compiler::*cc*
				  "-I/usr/include/FTGL -I/usr/include/freetype2"))

(defmacro compile-gfile (file-name)
  `(compile-file ,file-name :system-p t))

(defun compile-gacela ()
  (compile-gfile "gacela.lisp")
  (compile-gfile "gacela_misc.lisp")
  (compile-gfile "gacela_SDL.lisp")
  (compile-gfile "gacela_GL.lisp")
  (compile-gfile "gacela_FTGL.lisp")
  (compile-gfile "gacela_draw.lisp")
  (compile-gfile "gacela_ttf.lisp")
  (compile-gfile "gacela_events.lisp")
  (compile-gfile "gacela_mobs.lisp")
  (compile-gfile "gacela_widgets.lisp"))

(defun link-gacela ()
  (compiler::link
   '("gacela.o" "gacela_misc.o" "gacela_SDL.o" "gacela_GL.o" "gacela_FTGL.o" "gacela_draw.o" "gacela_ttf.o" "gacela_events.o" "gacela_mobs.o" "gacela_widgets.o")
   "gacela"
   ""
   "-lSDL -lSDL_image -lSDL_ttf -lSDL_mixer -lSDL_gfx -lGL -lGLU -lftgl"))

(defun build-gacela ()
  (compile-gacela)
  (link-gacela))
