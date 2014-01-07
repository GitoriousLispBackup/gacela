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


(define-module (gacela engine)
  #:use-module (gacela system)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 threads)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu))


;;; Engine Properties

(define* (get-property property-path #:key (engine (current-engine)))
  (let ((entities (get-entities-by-components (list (car property-path)) #:engine engine)))
    (cond ((null? entities)
	   #f)
	  (else
	   (let loop ((property (get-component (car property-path) (car entities)))
		      (path (cdr property-path)))
	     (cond ((or (null? path) (not property))
		    property)
		   (else
		    (loop (assoc-ref property (car path)) (cdr path)))))))))

(define* (set-property! property-path value #:key (engine (current-engine)))
  (define (loop property path)
    (cond ((null? path)
	   value)
	  (else
	   (assoc-set! (or property '()) (car path) (loop (assoc-ref property (car path)) (cdr path))))))

  (let ((entities (get-entities-by-components (list (car property-path)) #:engine engine)))
    (cond ((null? entities)
	   (new-entity! `(,(car property-path) . ,(loop '() (cdr property-path)))))
	  (else
	   (set-entity-components! (get-key (car entities)) `(,(car property-path) . ,(loop (get-component (car property-path) (car entities)) (cdr property-path))))))))

(export get-property
	set-property!)


;;; Engine Inner Properties

(define (default-delay) 0.1)

(define (default-engine-inner-properties)
  `(engine-inner-properties (delay . ,(default-delay))))


;;; Engine definitions

(define-record-type engine
  (make-engine-record entities mutex running-mutex system)
  engine?
  (entities engine-entities set-engine-entities!)
  (mutex engine-mutex set-engine-mutex!)
  (running-mutex engine-running-mutex set-engine-running-mutex!)
  (system engine-system set-engine-system!))

(set-record-type-printer! engine
  (lambda (record port)
    (format port "#<[engine] state: ~a, entities: ~a>"
	    (if (engine-running? record) "Running" "Stopped")
	    (length (car (engine-entities record))))))

(define (make-engine . systems)
  (make-engine-record
   (receive (e c) ((new-entity (default-engine-inner-properties)) '() '())
     (list e c))
   (make-mutex)
   (make-mutex)
   (apply group-systems systems)))

(define-syntax define-engine
  (syntax-rules ()
    ((_ name system ...)
     (define name
       (make-engine system ...)))))

(define (engine-running? engine)
  (mutex-locked? (engine-running-mutex engine)))

(export make-engine
	define-engine
	engine-running?)


;;; Engine Access Protocol Interface

(define current-engine-mutex (make-mutex))
(define current-engine-list '())

(define (current-engine)
  (with-mutex current-engine-mutex
    (assoc-ref current-engine-list (current-thread))))

(define (set-current-engine! engine)
  (with-mutex current-engine-mutex
    (set! current-engine-list
	  (cond (engine
		 (assoc-set! current-engine-list (current-thread) engine))
		(else
		 (assoc-remove! current-engine-list (current-thread)))))))

(define* (get-entity key #:key (engine (current-engine)))
  (assoc key (car (engine-entities engine))))

(define* (get-entities-by-components component-types #:key (engine (current-engine)))
  (map (lambda (e)
	 (get-entity e #:engine engine))
       (find-entities-by-components (cadr (engine-entities engine)) component-types)))

(define-syntax define-entity-setter
  (syntax-rules ()
    ((_ name! name)
     (define (name! . args)
       (let ((f (apply name args))
	     (engine (current-engine)))
	 (receive (e c r) (f (car (engine-entities engine)) (cadr (engine-entities engine)))
	   (set-engine-entities! engine (list e c))
	   r))))))

(define-entity-setter new-entity! new-entity)
(define-entity-setter remove-entity! remove-entity)
(define-entity-setter set-entity! set-entity)
(define-entity-setter set-entity-components! set-entity-components)
(define-entity-setter remove-entity-components! remove-entity-components)

(define-syntax with-engine
  (syntax-rules ()
    ((_ engine body ...)
     (let ((old-engine (current-engine)))
       (set-current-engine! engine)
       (let ((res (with-mutex (engine-mutex engine)
		    body
		    ...)))
	 (set-current-engine! old-engine)
	 res)))))

(define (set-engine-systems! engine . systems)
  (with-mutex (engine-mutex engine)
    (set-engine-system! engine (apply group-systems systems))))

(export current-engine
	set-current-engine!
	get-entity
	get-entities-by-components
	new-entity!
	remove-entity!
	set-entity!
	set-entity-components!
	remove-entity-components!
	with-engine
	set-engine-systems!)


;;; Engine execution

(define (start-engine engine)
  (cond ((not (engine-running? engine))
	 (with-mutex (engine-running-mutex engine)
	   (let loop ()
	     (let ((delay 0))
	       (with-engine engine
	         (receive (e c) ((apply (engine-system engine) (engine-entities engine)))
		   (set-engine-entities! engine (list e c)))
		 (set! delay (get-property '(engine-inner-properties delay))))
	       (usleep (inexact->exact (* delay 1000000))))
	     (if (not (engine-stopping? engine #:clean #t))
		 (loop)))))))

(define (stop-engine engine)
  (with-engine engine
    (new-entity! '(engine-halt . #t)))
  'engine-halt)

(define* (engine-stopping? engine #:key (clean #f))
  (let ((halt #f))
    (with-engine engine
      (let halt-engine ((halts (get-entities-by-components '(engine-halt))))
	(cond ((not (null? halts))
	       (set! halt #t)
	       (cond (clean
		      (remove-entity! (caar halts))
		      (halt-engine (cdr halts))))))))
    halt))

(export start-engine
	stop-engine
	engine-stopping?)
