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



;;; Additional modules

(use-modules (srfi srfi-1))


;;; Functions

(define (nearest-power-of-two n)
  (define (power p n)
    (cond ((> (* p 2) n) p)
	  (else (power (* p 2) n))))
  (power 1 n))

(define-macro (pushnew elem list)
  `(cond ((not (find (lambda (e) (eq? e ,elem)) ,list))
	  (set! ,list (cons ,elem ,list)))))
