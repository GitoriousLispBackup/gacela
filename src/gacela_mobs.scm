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

(define add-mob #f)
(define kill-mob #f)
(define get-active-mobs #f)
(define reload-mobs? #f)

(let ((active-mobs '(m1 m2)) (reload #f))
  (set! add-mob
	(lambda (mob)
	  (pushnew (procedure-name mob) active-mobs)
	  (set! reload #t)))

  (set! kill-mob
	(lambda (mob)
	  (set! active-mobs (lset-difference eq? active-mobs (list (procedure-name mob))))
	  (set! reload #t)))

  (set! get-active-mobs
	(lambda () active-mobs))

  (set! reload-mobs?
	(lambda () reload)))


(define-macro (get-mobs-function)
  (let ((mobs (get-active-mobs)))
    `(lambda () ,@(map (lambda (mob) `(,mob)) mobs))))

(define-macro (define-mob mob-head . look)
  (let ((name (car mob-head)) (attr (cdr mob-head)))
    `(begin
       (define ,name #f)
       (let ((attr ,attr))
	 (set! ,name
	       (lambda (option)
		 (case option
		   ((#:render)
		    (glPushMatrix)
		    ,@(map (lambda (x) (if (string? x) `(draw-image ,x) x)) look)
		    (glPopMatrix)))))))))
