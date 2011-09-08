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

(define show-mob-hash #f)
(define hide-mob-hash #f)
(define get-active-mobs #f)
(define clear-active-mobs #f)
(define mobs-changed? #f)

(let ((active-mobs (make-hash-table)) (changed #f))
  (set! show-mob-hash
	(lambda (key mob)
	  (hash-set! active-mobs key mob)
	  (set! changed #t)))

  (set! hide-mob-hash
	(lambda (key)
	  (hash-remove! key)
	  (set! changed #t)))

  (set! get-active-mobs
	(lambda* (#:optional (refreshed #t))
	  (set! changed (not refreshed))
	  (hash-map->list (lambda (k v) v) active-mobs)))

  (set! clear-active-mobs
	(lambda ()
	  (set! changed #t)
	  (hash-clear! active-mobs)))

  (set! mobs-changed?
	(lambda () changed)))


(define-macro (show-mob mob)
  (cond ((list? mob)
	 `(let ((m ,mob))
	    (show-mob-hash (m 'get-mob-id) m)))
	(else
	 `(show-mob-hash (,mob 'get-mob-id) (lambda () (,mob))))))

(define-macro (hide-mob mob)
  `(hide-mob-hash ',mob))

(define (run-mobs mobs)
  (for-each
   (lambda (m)
     (glPushMatrix)
     (m)
     (glPopMatrix))
   mobs))


;;; Making mobs

(define-macro (define-mob mob-head . body)
  (let ((name (car mob-head)) (attr (cdr mob-head)))
    `(define ,(string->symbol (string-concatenate (list "make-" (symbol->string name))))
       (lambda ()
	 (lambda-mob ,attr ,@body)))))

(define-macro (lambda-mob attr . body)
  `(let ,(cons '(mob-id (gensym)) attr)
     (lambda* (#:optional (option #f))
       (case option
	 ((get-mob-id)
	  mob-id)
	 (else
	  (catch #t
		 (lambda () ,@body)
		 (lambda (key . args) #f)))))))
