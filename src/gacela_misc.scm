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


;;; Constants

(define *pi* (* (asin 1) 2))


;;; Geometry

(define (degrees-to-radians angle)
  (/ (* angle *pi*) 180))

(define (radians-to-degrees angle)
  (/ (* angle 180) *pi*))

(define (distance-between-points p1 p2)
  (define (add-power-of-two p1 p2)
    (cond ((null? p1)
	   0)
	  (else
	   (+ (expt (- (car p1) (car p2)) 2)
	      (add-power-of-two (cdr p1) (cdr p2))))))

  (cond ((not (= (length p1) (length p2)))
	 #f)
	(else
	 (sqrt (add-power-of-two p1 p2)))))


;;; Functions

(define (nearest-power-of-two n)
  (define (power p n)
    (cond ((> (* p 2) n) p)
	  (else (power (* p 2) n))))
  (power 1 n))

(define-macro (pushnew elem list)
  `(cond ((not (find (lambda (e) (eq? e ,elem)) ,list))
	  (set! ,list (cons ,elem ,list)))))

(define (assoc-multiple-set! alist . pairs)
  (define (amset! alist pairs)
    (cond ((< (length pairs) 2)
	   alist)
	  (else
	   (assoc-set! alist (car pairs) (cadr pairs))
	   (amset! alist (cddr pairs)))))
  (amset! alist pairs))
