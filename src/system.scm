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
    (define (filtered-args args)
      (let ((datum (map (lambda (a) (syntax->datum a)) args)))
	(map (lambda (a) (datum->syntax x a))
	     (map (lambda (a) (if (list? a) (car a) a))
		  (filter (lambda (a) (not (keyword? a))) datum)))))
    (syntax-case x ()
      ((_ name field ...)
       (with-syntax ((make-name (concat "make-" #'name))
		     (make-name-record (concat "make-" #'name "-record"))
		     (name? (concat #'name "?"))
		     ((field-name ...) (filtered-args #'(field ...)))
		     ((field-getter ...) (map (lambda (f) (concat #'name "-" f)) (filtered-args #'(field ...))))
		     ((field-setter ...) (map (lambda (f) (concat "set-" #'name "-" f "!")) (filtered-args #'(field ...)))))
         #'(begin
	     (define* (make-name field ...)
	       (make-name-record field-name ...))
	     (define-record-type name
	       (make-name-record field-name ...)
	       name?
	       (field-name field-getter field-setter)
	       ...)
	     (set-record-type-printer! name
	       (lambda (record port)
		 (format port "#<[~a]" 'name)
		 (format port " ~a: ~a" 'field-name (field-getter record))
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

(define (make-entity-set . changes)
  (modify-entities
   (cons (make-hash-table) (make-hash-table))
   changes))

(define (entity-list entity-set)
  (hash-map->list (lambda (k v) (cons k v)) (car entity-set)))

(define (entity-count entity-set)
  (hash-count (const #t) (car entity-set)))

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
		(elist (hash-ref clist type)))
	   (hash-set! clist type
	     (cond (elist
		    (lset-adjoin eq? elist entity))
		   (else
		    (list entity))))
	   (register-components entity (cdr components) clist)))))

(define (unregister-components entity components clist)
  (cond ((null? components) clist)
	(else
	 (let* ((type (car components))
		(elist (lset-difference eq? (hash-ref clist type) (list entity))))
	   (cond ((null? elist)
		  (hash-remove! clist type))
		 (else
		  (hash-set! clist type elist)))
	   (unregister-components entity (cdr components) clist)))))

(define (component-names components)
  (map (lambda (c) (car c)) components))

(define (entity-component-names key entity-set)
  (component-names
   (hash-ref (car entity-set) key)))

(define (entity-ref key entity-set)
  (hash-get-handle (car entity-set) key))

(define (new-entity . new-components)
  (lambda (entity-set)
    (let ((key (gensym))
	  (nc (normalize-components new-components)))
      (hash-set! (car entity-set) key nc)
      (register-components key (component-names nc) (cdr entity-set))
      (values
       entity-set
       (cons key nc)))))

(define (remove-entity key)
  (lambda (entity-set)
    (let ((clist (entity-component-names key entity-set))
	  (entity (entity-ref key entity-set)))
      (hash-remove! (car entity-set) key)
      (unregister-components key clist (cdr entity-set))
      (values
       entity-set
       entity))))

(define (set-entity key . new-components)
  (lambda (entity-set)
    (let* ((nc (normalize-components new-components))
	   (clist (entity-component-names key entity-set))
	   (nclist (component-names nc)))
      (hash-set! (car entity-set) key nc)
      (register-components key
       (lset-difference eq? nclist clist)
       (unregister-components key (lset-difference eq? clist nclist) (cdr entity-set)))
      (values
       entity-set
       (cons key nc)))))

(define (set-entity-components key . new-components)
  (lambda (entity-set)
    (let ((nc (normalize-components new-components))
	  (clist (alist-copy (hash-ref (car entity-set) key))))
      (for-each
       (lambda (c)
	 (set! clist (assoc-set! clist (car c) (cdr c))))
       nc)
      (hash-set! (car entity-set) key clist)
      (register-components key (component-names nc) (cdr entity-set))
      (values
       entity-set
       (cons key clist)))))

(define (remove-entity-components key . old-components)
  (lambda (entity-set)
    (let ((clist (alist-copy (hash-ref (car entity-set) key))))
      (for-each
       (lambda (c)
	 (set! clist (assoc-remove! clist c)))
       old-components)
      (hash-set! (car entity-set) key clist)
      (unregister-components key old-components (cdr entity-set))
      (values
       entity-set
       (cons key clist)))))

(define (modify-entities entity-set changes)
  (cond ((null? changes)
	 entity-set)
	(else
	 (modify-entities ((car changes) entity-set) (cdr changes)))))

(export make-entity-set
	entity-list
	entity-count
	new-entity
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

(define (append-changes changes)
  (entities-changes
   (apply append
	  (map get-entities-changes changes))))

(define (find-entities-by-components entity-set clist)
  (cond ((null? clist) '())
	(else
	 (let ((e (hash-ref (cdr entity-set) (car clist) '()))
	       (e* (find-entities-by-components entity-set (cdr clist))))
	   (if (null? e*)
	       e
	       (lset-intersection eq? e e*))))))

(define-syntax make-system
  (syntax-rules ()
    ((_ ((name (component-type ...)) ...) form ...)
     (lambda (entity-set)
       (let ((name (map (lambda (x)
			  (cons (car x)
				(filter (lambda (x)
					  (memq (car x) '(component-type ...)))
					(cdr x))))
			(map (lambda (x)
			       (entity-ref x entity-set))
			     (find-entities-by-components entity-set '(component-type ...)))))
	     ...)
	 form
	 ...)))))

(define-syntax define-system
  (syntax-rules ()
    ((_ system-name ((name (component-type ...)) ...) form ...)
     (define system-name
       (make-system ((name (component-type ...)) ...)
         form
	 ...)))))

(define (composed-systems-result results)
  (let ((changes (filter (lambda (r) (entities-changes? r)) results)))
    (cond ((null? changes)
	   (car results))
	  (else
	   (append-changes changes)))))

(define (join-systems . systems)
  (lambda (entity-set)
    (let run ((s systems) (res '()))
      (cond ((null? s)
	     (composed-systems-result res))
	    (else
	     (run (cdr s) (cons ((car s) entity-set) res)))))))

(define (thread-systems . systems)
  (lambda (entity-set)
    (let run-wait ((thd
		    (map (lambda (s)
			   (call-with-new-thread
			    (lambda () (s entity-set))))
			 systems))
		   (res '()))
      (cond ((null? thd)
	     (composed-systems-result res))
	    (else
	     (run-wait (cdr thd) (cons (join-thread (car thd)) res)))))))

(export entities-changes
	entities-changes?
	get-entities-changes
	find-entities-by-components
	define-system
	make-system
	join-systems
	thread-systems)


;;; Entities and components access inside systems

(define (get-key entity)
  (car entity))

(define (get-component component-name entity)
  (assoc-ref (cdr entity) component-name))

(export get-key
	get-component)
