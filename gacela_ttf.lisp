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

(defun open-font (font-file &optional (size 80) (encoding ft_encoding_unicode))
  (let ((font (ftglCreateTextureFont font-file)))
    (cond ((/= font 0)
	   (ftglSetFontFaceSize font size 72)
	   (ftglSetFontCharMap font encoding)
	   font))))

(defun render-text (text font)
  (ftglRenderFont font text FTGL_RENDER_ALL))
