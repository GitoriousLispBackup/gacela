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

(defun load-font (font-file &key (size 80) (encoding ft_encoding_unicode) static)
  (let ((key (make-resource-font :filename font-file :size size :encoding encoding)))
    (cond ((get-resource key) key)
	  (t (true-load-font font-file size encoding static)))))

(defun true-load-font (font-file size encoding static)
  (let ((key (make-resource-font :filename font-file :size size :encoding encoding))
	(font (ftglCreateTextureFont font-file)))
    (cond ((/= font 0)
	   (ftglSetFontFaceSize font size 72)
	   (ftglSetFontCharMap font encoding)
	   (set-resource key
			 `(:id-font ,font)
			 (lambda () (true-load-font font-file size encoding static))
			 (lambda () (ftglDestroyFont font))
			 :static static)
	   key))))

(defun render-text (text font)
  (ftglRenderFont (getf (get-resource font) :id-font) text FTGL_RENDER_ALL))
