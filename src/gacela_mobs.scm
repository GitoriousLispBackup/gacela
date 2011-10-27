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
(define refresh-active-mobs #f)
(define get-active-mobs #f)
(define hide-all-mobs #f)
(define mobs-changed? #f)

(let ((mobs-table (make-hash-table))
      (active-mobs '())
      (changed #f))

  (set! show-mob-hash
	(lambda (mob)
	  (hash-set! mobs-table (mob 'get-mob-id) mob)
	  (set! changed #t)))

  (set! hide-mob-hash
	(lambda (mob-id)
	  (hash-remove! mobs-table mob-id)
	  (set! changed #t)))

  (set! refresh-active-mobs
	(lambda ()
	  (cond (changed
		 (set! changed #f)
		 (set! active-mobs (hash-map->list (lambda (k v) v) mobs-table))))))

  (set! get-active-mobs
	(lambda () active-mobs))

  (set! hide-all-mobs
	(lambda ()
	  (set! changed #t)
	  (hash-clear! mobs-table)))

  (set! mobs-changed?
	(lambda () changed)))


(define-macro (show-mob mob)
  (cond ((list? mob)
	 `(let ((m ,mob))
	    (show-mob-hash m)))
	(else
	 `(show-mob-hash (lambda* (#:optional (option #f)) (,mob option))))))

(define-macro (hide-mob mob)
  (cond ((list? mob)
	 `(let ((m ,mob))
	    (hide-mob-hash (m 'get-mob-id))))
	(else
	 `(hide-mob-hash (,mob 'get-mob-id)))))

(define* (run-mobs #:optional (mobs (get-active-mobs)))
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
       (lambda* ,(if (null? attr) '() `(#:key ,@attr))
	 (the-mob ',name () ,attr ,@body)))))

(define-macro (the-mob type attr publish . body)
  (let ((mob-id-symbol (gensym))
	(type-symbol (gensym))
	(time-symbol (gensym))
	(data-symbol (gensym))
	(save-symbol (gensym)))
    `(let ((,mob-id-symbol (gensym))
	   (,type-symbol ,type)
	   (,time-symbol 0)
	   (,data-symbol '())
	   ,@attr)
       (lambda* (#:optional (option #f))
	 (define (kill-me)
	   (hide-mob-hash ,mob-id-symbol))
	 (define (,save-symbol)
	   (let ((time (get-frame-time)))
	     (cond ((not (= time ,time-symbol))
		    (set! ,time-symbol time)
		    (set! ,data-symbol ,(cons 'list (map (lambda (x) `(cons ',(car x) ,(car x))) publish)))))))
	 (define (map-mobs fun type)
	   (let ((mobs (filter (lambda (m) (and (eq? (m 'get-type) type) (not (eq? (m 'get-mob-id) ,mob-id-symbol)))) (get-active-mobs))))
	     (map (lambda (m) (fun (m 'get-data))) mobs)))
	 (case option
	   ((get-mob-id)
	    ,mob-id-symbol)
	   ((get-type)
	    ,type-symbol)
	   ((get-data)
	    (,save-symbol)
	    ,data-symbol)
	   (else
	    (,save-symbol)
	    (catch #t
	     	   (lambda () ,@body)
		   (lambda (key . args) #f))))))))

(define-macro (lambda-mob attr . body)
  `(the-mob 'undefined ,attr '() ,@body))
