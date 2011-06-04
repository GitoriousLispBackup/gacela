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

(define-macro (hash-add-mob hash-table mob)
  `(hash-set! ,hash-table (procedure-name ,mob) (lambda () (,mob))))

(define-macro (hash-remove-mob hash-table mob)
  `(hash-remove! ,hash-table (procedure-name ,mob)))


(define add-mob #f)
(define kill-mob #f)
(define get-mob-world #f)
(define mobs #f)
(define vreload #f)

(let ((active-mobs (make-hash-table)) (reload #f))
  (set! add-mob
	(lambda (mob)
	  (hash-add-mob active-mobs mob)
	  (set! reload #t)))

  (set! kill-mob
	(lambda (mob)
	  (hash-remove-mob active-mobs mob)
	  (set! reload #t)))

  (set! get-mob-world
	(lambda ()
	  (cond (reload
		 (set! reload #f)
		 (hash-map->list (lambda (k v) v) active-mobs))
		(else #f))))

  (set! mobs (lambda () active-mobs))
  (set! vreload (lambda () reload)))
