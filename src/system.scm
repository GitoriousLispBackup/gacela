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
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu))


;;; Component definitions

(define (symbol-concatenate . args)
  (string->symbol
   (string-concatenate
    (map (lambda (a) (if (symbol? a) (symbol->string a) a)) args))))

(define-syntax define-component
  (lambda (x)
    (define (concat . args)
      (datum->syntax x
        (apply symbol-concatenate
	  (map (lambda (a)
		 (if (string? a)
		     a
		     (syntax->datum a)))
	       args))))
    (syntax-case x ()
      ((_ name field ...)
       (with-syntax ((make-name (concat "make-" #'name))
		     (name? (concat #'name "?"))
		     ((field-getter ...) (map (lambda (f) (concat #'name "-" f)) #'(field ...)))
		     ((field-setter ...) (map (lambda (f) (concat "set-" #'name "-" f "!")) #'(field ...))))
         #'(begin
	     (define-record-type name
	       (make-name field ...)
	       name?
	       (field field-getter field-setter)
	       ...)
	     (set-record-type-printer! name
	       (lambda (record port)
		 (format port "#<[~a]" 'name)
		 (format port " ~a: ~a" 'field (field-getter record))
		 ...
		 (format port ">")))
	     'name))))))

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
       (cons key nc)))))

(define (remove-entity key)
  (lambda (entities components)
    (let ((clist (map (lambda (c) (car c)) (assoc-ref entities key)))
	  (entity (assoc key entities)))
      (values
       (assoc-remove! entities key)
       (unregister-components key clist components)
       entity))))

(define (set-entity key . new-components)
  (lambda (entities components)
    (let* ((nc (normalize-components new-components))
	   (clist (map (lambda (c) (car c)) (assoc-ref entities key)))
	   (nclist (map (lambda (c) (car c)) nc)))
      (values
       (assoc-set! entities key nc)
       (register-components key (lset-difference eq? nclist clist)
			    (unregister-components key (lset-difference eq? clist nclist) components))
       (cons key nc)))))

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
       (register-components key (map (lambda (c) (car c)) nc) components)
       (cons key clist)))))

(define (remove-entity-components key . old-components)
  (lambda (entities components)
    (let ((clist (alist-copy (assoc-ref entities key))))
      (for-each
       (lambda (c)
	 (assoc-remove! clist c))
       old-components)
      (values
       (assoc-set! entities key clist)
       (unregister-components key old-components components)
       (cons key clist)))))

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

(define-record-type entities-changes-type
  (entities-changes changes)
  entities-changes?
  (changes get-entities-changes))

(define* (find-entities-by-components c t)
  (cond ((null? t) '())
	(else
	 (let* ((e (assoc-ref c (car t)))
		(e* (if e e '())))
	   (cond ((null? (cdr t)) e*)
		 (else
		  (lset-intersection eq? e* (find-entities-by-components c (cdr t)))))))))

(define-syntax make-system
  (syntax-rules ()
    ((_ ((name (component-type ...)) ...) form ...)
     (lambda (entities components)
       (let ((name (map (lambda (x)
			  (cons (car x)
				(filter (lambda (x)
					  (memq (car x) '(component-type ...)))
					(cdr x))))
			(map (lambda (x)
			       (assoc x entities))
			     (find-entities-by-components components '(component-type ...)))))
	     ...)
	 (let ((res (begin form ...)))
	   (lambda* (#:optional (entities2 #f) (components2 #f))
	     (let ((e (if (and entities2 components2) entities2 entities))
		   (c (if (and entities2 components2) components2 components)))
	       (modify-entities (if (entities-changes? res) (get-entities-changes res) '()) e c)))))))))

(define-syntax define-system
  (syntax-rules ()
    ((_ system-name ((name (component-type ...)) ...) form ...)
     (define system-name
       (make-system ((name (component-type ...)) ...)
         form
	 ...)))))

(define (join-systems . systems)
  (lambda (entities components)
    (let ((changes
	   (let run ((s systems) (e (alist-copy entities)) (c (alist-copy components)) (res '()))
	     (cond ((null? s)
		    res)
		   (else
		    (let ((r ((car s) e c)))
		      (receive (e2 c2) (r)
		        (run (cdr s) e2 c2 (cons r res)))))))))
      (lambda* (#:optional (entities2 #f) (components2 #f))
        (let modify ((e (if (and entities2 components2) entities2 entities))
		     (c (if (and entities2 components2) components2 components))
		     (ch (reverse changes)))
	  (cond ((null? ch)
		 (values e c))
		(else
		 (receive (e2 c2) ((car ch) e c)
		   (modify e2 c2 (cdr ch))))))))))

(define (threaded-systems . systems)
  (lambda (entities components)
    (let ((changes
	   (let run-wait ((thd
			   (map (lambda (s)
				  (call-with-new-thread
				   (lambda () (s entities components))))
				systems))
			  (res '()))
	     (cond ((null? thd)
		    res)
		   (else
		    (run-wait (cdr thd) (cons (join-thread (car thd)) res)))))))
      (lambda* (#:optional (entities2 #f) (components2 #f))
        (let modify ((e (if (and entities2 components2) entities2 entities))
		     (c (if (and entities2 components2) components2 components))
		     (ch changes))
	  (cond ((null? ch)
		 (values e c))
		(else
		 (receive (e2 c2) ((car ch) e c)
		   (modify e2 c2 (cdr ch))))))))))

(define (group-systems . systems)
  (cond ((null? systems)
	 (make-system ()))
	((= (length systems) 1)
	 (car systems))
	(else
	 (apply join-systems systems))))

(export entities-changes
	entities-changes?
	get-entities-changes
	find-entities-by-components
	define-system
	make-system
	join-systems
	threaded-systems
	group-systems)


;;; Entities and components access inside systems

(define (get-key entity)
  (car entity))

(define (get-component component-name entity)
  (assoc-ref (cdr entity) component-name))

(export get-key
	get-component)
