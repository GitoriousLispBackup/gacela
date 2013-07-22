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


(define-module (gacela test)
  #:use-module (gacela system)
  #:use-module (ice-9 receive))


(define-component a x y)
(define-component b)
(define-component c)

(export-component a)

(define (test1)
  (let ((entities '())
	(components '())
	(key #f))
    (receive (e c k) (new-entity `(,(make-a 1 2) ,(make-b)) entities components)
	     (set! entities e)
	     (set! components c)
	     (set! key k)
	     (display k) (newline))
    (format #t "~a~%~a~%~%" entities components)

    (receive (e c k) (new-entity `(,(make-a 10 20)) entities components)
	     (set! entities e)
	     (set! components c)
	     (display k) (newline))
    (format #t "~a~%~a~%~%" entities components)

    (receive (e c) (remove-entity key entities components)
	     (set! entities e)
	     (set! components c))
    (format #t "~a~%~a~%~%" entities components)
))

(export test1)
