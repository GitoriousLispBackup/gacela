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
  #:use-module (gacela misc)
  #:use-module (gacela system)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 threads)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu))


;;; Engine Inner Properties

(define (default-step) 0.1)

(define (default-engine-inner-properties)
  `(engine-inner-properties (step . ,(default-step))))


;;; Engine definitions

(define-record-type engine
  (make-engine-record entities mutex running-mutex systems)
  engine?
  (entities engine-entities set-engine-entities!)
  (mutex engine-mutex set-engine-mutex!)
  (running-mutex engine-running-mutex set-engine-running-mutex!)
  (systems engine-systems set-engine-systems!))

(set-record-type-printer! engine
  (lambda (record port)
    (format port "#<[engine] state: ~a, entities: ~a>"
	    (if (engine-running? record) "Running" "Stopped")
	    (entity-count (engine-entities record)))))

(define (make-engine . systems)
  (make-engine-record
   (make-entity-set (new-entity (default-engine-inner-properties)))
   (make-mutex)
   (make-mutex)
   systems))

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


;;; Engine execution

(define (start-engine engine)
  (cond ((not (engine-running? engine))
	 (with-mutex (engine-running-mutex engine)
	   (let loop ()
	     (let ((t (current-utime))
		   (delay 0)
		   (halt #f))
	       (with-mutex (engine-mutex engine)
	         (for-each
		  (lambda (s) (eval-system s engine))
		  (engine-systems engine))
		 (set! delay (- (inexact->exact (* (engine-property engine 'step) 1000000))
				(- (current-utime) t)))
		 (set! halt (engine-stopping? engine #:clean #t)))
	       (cond ((not halt)
		      (cond ((> delay 0)
			     (usleep delay)))
		      (loop)))))))))

(define (eval-system system engine)
  (call-with-values
      (lambda () (system (engine-entities engine)))
    (lambda vals
      (let ((changes (car vals)))
	(cond ((entities-changes? changes)
	       (set-engine-entities! engine
	         (modify-entities (engine-entities engine)
				  (get-entities-changes changes))))))
      (apply values vals))))

(define-syntax with-engine
  (syntax-rules ()
    ((_ engine component-types form ...)
     (with-mutex (engine-mutex engine)
       (eval-system (make-system component-types form ...) engine)))))

(define (stop-engine engine)
  (with-engine engine ()
    (entities-changes
     (list
      (new-entity '(engine-halt . #t)))))
  'engine-halt)

(define* (engine-stopping? engine #:key (clean #f))
  (let ((halts (eval-system
		(make-system ((halt (engine-halt))) halt)
		engine)))
    (cond ((and clean (not (null? halts)))
	   (eval-system
	    (make-system () (entities-changes (map (lambda (h) (remove-entity (car h))) halts)))
	    engine)))
    (not (null? halts))))

(export start-engine
	with-engine
	stop-engine)


;;; Properties

(define (engine-property engine name)
  (eval-system
   (make-system ((props (engine-inner-properties)))
     (assoc-ref
      (assoc-ref (cdar props) 'engine-inner-properties)
      name))
   engine))

(define (set-engine-property! engine name value)
  (eval-system
   (make-system ((props (engine-inner-properties)))
     (entities-changes
      (list
       (set-entity (caar props)
		   (car
		    (assoc-set! (cdar props) 'engine-inner-properties
				(assoc-set! (assoc-ref (cdar props) 'engine-inner-properties)
					    name
					    value)))))))
   engine)
  value)

(export engine-property
	set-engine-property!)
