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

(defmacro mapcconst (type c-type name)
  (let ((c-header (concatenate 'string c-type " gacela_" name " (void)"))
	(c-body (concatenate 'string "return " name ";"))
	(c-name (concatenate 'string "gacela_" name))
	(lisp-name (intern (string-upcase name))))
    `(progn
       (defcfun ,c-header 0 ,c-body)
       (defentry ,lisp-name () (,type ,c-name))
       (eval-when (load) (defconstant ,lisp-name (,lisp-name))))))

(clines "#include <FTGL/ftgl.h>")

(mapcconst int "int" "ft_encoding_unicode")
(mapcconst int "int" "FTGL_RENDER_ALL")

;;; FTGL Functions
(defcfun "int gacela_ftglCreateTextureFont (char *file)" 0
  "return ftglCreateTextureFont (file);")

(defcfun "int gacela_ftglSetFontFaceSize (int font, int size, int res)" 0
  "return ftglSetFontFaceSize (font, size, res);")

(defcfun "int gacela_ftglSetFontCharMap (int font, int encoding)" 0
  "return ftglSetFontCharMap (font, encoding);")

(defcfun "void gacela_ftglRenderFont (int font, char *string, int mode)" 0
  "ftglRenderFont (font, string, mode);")

(defcfun "void gacela_ftglDestroyFont (int font)" 0
  "ftglDestroyFont (font);")

(defentry ftglCreateTextureFont (string) (int "gacela_ftglCreateTextureFont"))
(defentry ftglSetFontFaceSize (int int int) (int "gacela_ftglSetFontFaceSize"))
(defentry ftglSetFontCharMap (int int) (int "gacela_ftglSetFontCharMap"))
(defentry ftglRenderFont (int string int) (void "gacela_ftglRenderFont"))
(defentry ftglDestroyFont (int) (void "gacela_ftglDestroyFont"))
