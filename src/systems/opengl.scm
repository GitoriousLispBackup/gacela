;;; Gacela, a GNU Guile extension for fast games development
;;; Copyright (C) 2013 by Javier Sancho Fernandez <jsf at jsancho dot org>
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


(define-module (gacela systems opengl)
  #:use-module (figl gl)
  #:use-module (figl glu)
  #:use-module (figl glut)
  #:use-module (gacela system))

(define-component opengl-window title #:optional window-handle)

(define-system windows-manager ((windows (window-handle)))
  (display windows)
  (newline))

(export-component opengl-window)
(export windows-manager)
