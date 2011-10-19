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
  (run-mobs-events)
  (for-each (lambda (m) (m 'publish-data)) mobs)
  (run-mobs-events)
  (for-each
   (lambda (m)
     (glPushMatrix)
     (m)
     (glPopMatrix))
   mobs)
  (clear-events-data))


;;; Making mobs

(define-macro (define-mob mob-head . body)
  (let ((name (car mob-head)) (attr (cdr mob-head)))
    `(define ,(string->symbol (string-concatenate (list "make-" (symbol->string name))))
       (lambda* ,(if (null? attr) '() `(#:key ,@attr))
	 (the-mob ',name () ,attr ,@body)))))

(define-macro (the-mob type attr publish . body)
  (let ((mob-id-symbol (gensym))
	(type-symbol (gensym)))
    `(let ((,mob-id-symbol (gensym))
	   (,type-symbol ,type)
	   ,@attr)
       (lambda* (#:optional (option #f))
	 (define (kill-me)
	   (hide-mob-hash ,mob-id-symbol))
	 (case option
	   ((get-mob-id)
	    ,mob-id-symbol)
	   ((get-type)
	    ,type-symbol)
	   ((get-data)
	    ,(cons 'list (map (lambda (x) `(cons ',(car x) ,(car x))) publish)))
	   (else
	    (catch #t
		   (lambda () ,@body)
		   (lambda (key . args) #f))))))))

(define-macro (lambda-mob attr . body)
  `(the-mob 'undefined ,attr '() ,@body))


;;; Events Engine

(define def-mobs-event #f)
(define run-mobs-events #f)
(define clear-events-data #f)

(define mobs-events (make-hash-table))
(define returned-data (make-hash-table))

(let ((nop #f))
  (set! def-mobs-event
	(lambda (pair fun)
	  (cond ((not fun)
		 (hash-remove! mobs-events pair))
		(else
		 (hash-set! mobs-events pair fun)))))

  (set! run-mobs-events
	(lambda* (#:optional (mobs (get-active-mobs)))
	  (hash-for-each
	   (lambda (types fun)
	     (let* ((t1 (car types)) (t2 (cadr types))
		    (mobs-t1 (filter (lambda (m) (eq? (m 'get-type) t1)) mobs))
		    (mobs-t2 (filter (lambda (m) (eq? (m 'get-type) t2)) mobs)))
	       (cond ((not (or (null? mobs-t1) (null? mobs-t2)))
		      (for-each
		       (lambda (m1)
			 (let ((id1 (m1 'get-mob-id)))
			   (for-each
			    (lambda (m2)
			      (let ((id2 (m2 'get-mob-id)))
				(cond ((not (eq? id1 id2))
				       (let ((res (catch #t
							 (lambda () (fun (m1 'get-data) (m2 'get-data)))
							 (lambda (key . args) #f))))
					 (cond ((and (list? res) (>= (length res) 2))
						(return-data id1 t2 (car res))
						(return-data id2 t1 (cadr res)))))))))
			    mobs-t2)))
		       mobs-t1)))))
	   mobs-events)))
		  

  (define (return-data mob-id mob-type data)
    (let* ((key (list mob-id mob-type))
	   (res (hash-ref returned-data key)))
      (hash-set! returned-data key
		 (cond (res (cons data res))
		       (else (list data))))))

  (set! clear-events-data
	(lambda ()
	  (hash-clear! returned-data))))

(define-macro (define-mobs-event types-pair . body)
  `(def-mobs-event
     ',types-pair
     ,(cond ((null? body) #f)
	    (else `(lambda (,(car types-pair) ,(cadr types-pair)) ,@body)))))
