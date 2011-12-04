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


(define-module (gacela ttf)
  #:use-module (gacela ftgl))

(define* (load-font font-file #:key (size 40) (encoding ft_encoding_unicode))
  (let* ((key (list font-file))
	 (font (get-resource-from-cache key)))
    (cond ((not font)
	   (set! font (ftglCreateTextureFont font-file))
	   (insert-resource-into-cache key font)))
    (ftglSetFontFaceSize font size 72)
    (ftglSetFontCharMap font encoding)
    font))

(define* (render-text text font #:key (size #f))
  (cond (size (ftglSetFontFaceSize font size 72)))
  (ftglRenderFont font text FTGL_RENDER_ALL))
