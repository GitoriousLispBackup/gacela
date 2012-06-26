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


(define-module (gacela draw)
  #:use-module (gacela gacela)
  #:use-module ((gacela video) #:renamer (symbol-prefix-proc 'video:))
  #:use-module ((gacela gl) #:select (glPushMatrix glPopMatrix))
  #:use-module (ice-9 optargs))

(define-macro (define-view name content)
  `(begin
     (hash-set! active-views ',name (lambda () (video:glmatrix-block ,content)))
     ',name))

(define (square . params)
  (define-view tmp (apply video:draw-square params)))

(module-map (lambda (sym var)
	      (if (not (eq? sym '%module-public-interface))
		  (module-export! (current-module) (list sym))))
	    (current-module))
