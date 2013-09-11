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


(define-module (gacela examples making-systems)
  #:use-module (gacela system)
  #:use-module (ice-9 receive))


(define-component a x y)

(define (making-systems)
  (let ((entities '())
	(components '()))
    (receive (e c) (((make-system () (lambda (e) (list (new-entity (make-a 1 2)) (new-entity (make-a 10 20))))) entities components))
	     (set! entities e)
	     (set! components c))
    (format #t "Two new entities with a:~%~a~%~a~%~%" entities components)

    (((make-system (a) (lambda (e) (display e) (newline) '())) entities components))))

(export making-systems)
