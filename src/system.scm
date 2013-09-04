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


(define-module (gacela system)
  #:use-module (ice-9 receive)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9))


;;; Component definitions

(define (symbol-concatenate . args)
  (string->symbol
   (string-concatenate
    (map (lambda (a) (if (symbol? a) (symbol->string a) a)) args))))

(define-macro (define-component name . args)
  `(begin
     (use-modules (srfi srfi-9) (srfi srfi-9 gnu))
     (define-record-type ,name
       (,(symbol-concatenate "make-" name) ,@args)
       ,(symbol-concatenate name "?")
       ,@(map (lambda (a) (list a (symbol-concatenate name "-" a) (symbol-concatenate "set-" name "-" a "!"))) args))
     (set-record-type-printer! ,name
       (lambda (record port)
	 (format port "#<[~a]" ',name)
	 ,@(map (lambda (a) `(format port " ~a: ~a" ',a (,(symbol-concatenate name "-" a) record))) args)
	 (format port ">")))
     ',name))

(define (export-component component)
  (let ((name (record-type-name component))
	(m (current-module)))
    (module-export! m (list
		       (symbol-concatenate "make-" name)
		       (symbol-concatenate name "?")))
    (for-each
     (lambda (a)
       (module-export! (current-module)
		       (list
			(symbol-concatenate name "-" a)
			(symbol-concatenate "set-" name "-" a "!"))))
     (record-type-fields component))))

(define (get-component-type component)
  (record-type-name (record-type-descriptor component)))

(export define-component
	export-component
	get-component-type)


;;; Entities and components

(define (normalize-components components)
  (map
   (lambda (c)
     (if (record? c)
	 `(,(get-component-type c) . ,c)
	 c))
   components))

(define (register-components entity components clist)
  (cond ((null? components) clist)
	(else
	 (let* ((type (car components))
		(elist (assoc-ref clist type)))
	   (register-components entity (cdr components)
	     (assoc-set! clist type
	       (cond (elist
		      (lset-adjoin eq? elist entity))
		     (else
		      (list entity)))))))))

(define (unregister-components entity components clist)
  (cond ((null? components) clist)
	(else
	 (let* ((type (car components))
		(elist (lset-difference eq? (assoc-ref clist type) (list entity))))
	   (unregister-components entity (cdr components)
	     (cond ((null? elist)
		    (assoc-remove! clist type))
		   (else
		    (assoc-set! clist type elist))))))))

(define (new-entity . new-components)
  (lambda (entities components)
    (let ((key (gensym))
	  (nc (normalize-components new-components)))
      (values
       (acons key nc entities)
       (register-components key
			    (map (lambda (c) (car c)) nc)
			    components)
       key))))

(define (remove-entity key)
  (lambda (entities components)
    (let ((clist (map (lambda (c) (car c)) (assoc-ref entities key))))
      (values
       (assoc-remove! entities key)
       (unregister-components key clist components)))))

(define (set-entity key . new-components)
  (lambda (entities components)
    (let* ((nc (normalize-components new-components))
	   (clist (map (lambda (c) (car c)) (assoc-ref entities key)))
	   (nclist (map (lambda (c) (car c)) nc)))
      (values
       (assoc-set! entities key nc)
       (register-components key (lset-difference eq? nclist clist)
			    (unregister-components key (lset-difference eq? clist nclist) components))))))

(define (set-entity-components key . new-components)
  (lambda (entities components)
    (let ((nc (normalize-components new-components))
	  (clist (alist-copy (assoc-ref entities key))))
      (for-each
       (lambda (c)
	 (assoc-set! clist (car c) (cdr c)))
       nc)
      (values
       (assoc-set! entities key clist)
       (register-components key (map (lambda (c) (car c)) nc) components)))))

(define (remove-entity-components key . old-components)
  (lambda (entities components)
    (let ((clist (alist-copy (assoc-ref entities key))))
      (for-each
       (lambda (c)
	 (assoc-remove! clist c))
       old-components)
      (values
       (assoc-set! entities key clist)
       (unregister-components key old-components components)))))

(define (modify-entities changes entities components)
  (cond ((null? changes)
	 (values entities components))
	(else
	 (receive (e c) ((car changes) entities components)
	   (modify-entities (cdr changes) e c)))))

(export new-entity
	remove-entity
	set-entity
	set-entity-components
	remove-entity-components
	modify-entities)


;;; Making systems

(define* (find-entities-by-components c t)
  (cond ((null? t) '())
	(else
	 (let* ((e (assoc-ref c (car t)))
		(e* (if e e '())))
	   (cond ((null? (cdr t)) e*)
		 (else
		  (lset-intersection eq? e* (find-entities-by-components c (cdr t)))))))))
		  

(define (make-system component-types system-fun)
  (lambda (entities components)
    (let* ((e (find-entities-by-components components component-types))
	   (e* (map (lambda (x) (assoc x entities)) e))
	   (e** (map (lambda (x) (cons (car x) (filter (lambda (x) (memq (car x) component-types)) (cdr x)))) e*))
	   (res (system-fun e**)))
      (lambda* (#:optional (entities2 #f) (components2 #f))
	(let ((e (if (and entities2 components2) entities2 entities))
	      (c (if (and entities2 components2) components2 components)))
	  (modify-entities res e c))))))


(define (join-systems . systems)
  (lambda (entities components)
    (let run ((s systems) (e entities) (c components))
      (cond ((null? s)
	     (values e c))
	    (else
	     (receive (e2 c2) (((car s) e c))
	       (run (cdr s) e2 c2)))))))


(define (threaded-systems . systems)
  (lambda (entities components)
    (let run-wait ((thd
		    (map
		     (lambda (s)
		       (call-with-new-thread
			(lambda () (s entities components))))
		     systems))
		   (e entities) (c components))
      (cond ((null? thd)
	     (values e c))
	    (else
	     (receive (e2 c2) ((join-thread (car thd)) e c)
	       (run-wait (cdr thd) e2 c2)))))))


(export find-entities-by-components
	make-system
	join-systems
	threaded-systems)
