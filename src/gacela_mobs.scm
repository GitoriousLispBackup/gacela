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


;;; Mobs Factory

(define add-mob-lambda #f)
(define kill-mob-symbol #f)
(define get-active-mobs #f)
(define mobs-changed? #f)

(let ((active-mobs '()) (changed #f))
  (set! add-mob-lambda
	(lambda (mob)
	  (pushnew mob active-mobs)
	  (set! changed #t)))

  (set! kill-mob-symbol
	(lambda (mob)
	  (set! active-mobs (lset-difference eq? active-mobs (list mob)))
	  (set! changed #t)))

  (set! get-active-mobs
	(lambda* (#:optional (refreshed #t))
	  (set! changed (not refreshed))
	  active-mobs))

  (set! mobs-changed?
	(lambda () changed)))


(define-macro (add-mob mob)
  `(add-mob-lambda (lambda (option) (,mob option))))

(define-macro (kill-mob mob)
  `(kill-mob-symbol ',mob))

(define (process-mobs mobs)
  (for-each (lambda (m) (m #:render)) mobs))

(define-macro (define-mob mob-head . look)
  (let ((name (car mob-head)) (attr (cdr mob-head)))
    `(begin
       (define ,name #f)
       (let ((attr ',attr))
	 (set! ,name
	       (lambda (option)
		 (case option
		   ((#:render)
		    (glPushMatrix)
		    ,@(map (lambda (x) (if (string? x) `(draw-image ,x) x)) look)
		    (glPopMatrix)))))))))
