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

(define (new-entity new-components entities components)
  (let ((key (gensym)))
    (values
     (acons key (map (lambda (c) `(,(get-component-type c) . ,c)) new-components) entities)
     (register-components key new-components components)
     key)))

(define* (register-components entity components clist)
  (cond ((null? components) clist)
	(else
	 (let* ((type (get-component-type (car components)))
		(elist (assoc-ref clist type)))
	   (register-components entity (cdr components)
	     (assoc-set! clist type
	       (cond (elist
		      (lset-adjoin eq? elist entity))
		     (else
		      (list entity)))))))))

(export new-entity)


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
	   (e** (map (lambda (x) (cons (car x) (filter (lambda (x) (memq (get-component-type x) component-types)) (cdr x)))) e*))
	   (res (system-fun e**)))
      (lambda* (#:optional (entities2 #f) (components2 #f))
        (let* ((e2 (if (and entities2 components2)
		       (find-entities-by-components components2 component-types)
		       e))
	       (e2* (if (and entities2 components2)
			(map (lambda (x) (assoc x entities2)) e2)
			e*))
	       (e2** (if (and entities2 components2)
			 (map (lambda (x) (cons (car x) (filter (lambda (x) (memq (get-component-type x) component-types)) (cdr x)))) e2*)
			 e**)))
	  e2**)))))

; ((1 a b) (2 a b c) (3 c))
; ((1 a b) (2 a b))
; ((1 a) (a b))
; ((1 a) (3 c) (4 a b))

(export find-entities-by-components
	make-system)
