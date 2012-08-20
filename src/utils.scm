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


(define-module (gacela utils)
  #:export (use-cache-with
	    arguments-calling
	    arguments-apply
	    bound?))


;;; Cache for procedures

(define (use-cache-with proc)
  "Cache for procedures"
  (let ((cache (make-weak-value-hash-table)))
    (lambda (. param)
      (let* ((key param)
	     (res (hash-ref cache key)))
	(cond (res res)
	      (else
	       (set! res (apply proc param))
	       (hash-set! cache key res)
	       res))))))


;;; Playing with procedures arguments

(define undefined)
(define (bound? var) (not (eq? var undefined)))

(define (required-arguments args values)
  "Return an alist with required arguments and their values"
  (define (f vars values)
    (cond ((null? vars) '())
	  ((null? values) (assoc-set! (f (cdr vars) '())
				      (car vars)
				      undefined))
	  (else (assoc-set! (f (cdr vars) (cdr values))
			    (car vars)
			    (car values)))))
  (f (assoc-ref args 'required) values))

(define (optional-arguments args values)
  "Return an alist with optional arguments and their values"
  (define (f vars values)
    (cond ((null? vars) '())
	  ((null? values) (assoc-set! (f (cdr vars) '())
				      (car vars)
				      undefined))
	  (else (assoc-set! (f (cdr vars) (cdr values))
			    (car vars)
			    (car values)))))
  (f (assoc-ref args 'optional)
     (list-tail values
		(min (length (assoc-ref args 'required))
		     (length values)))))

(define (keyword-arguments args values)
  "Return an alist with keyword arguments and their values"
  (define (f vars values)
    (cond ((null? vars) '())
	  (else
	   (let ((val (member (car vars) values)))
	     (assoc-set! (f (cdr vars) values)
			    (keyword->symbol (car vars))
			    (if val (cadr val) undefined))))))
  (f (map (lambda (x) (car x)) (assoc-ref args 'keyword)) values))

(define (rest-arguments args values)
  "Return an alist with rest arguments"
  (let ((rest (assoc-ref args 'rest)))
    (cond (rest (assoc-set! '()
			    rest
			    (list-tail values
				       (min (+ (length (assoc-ref args 'required))
					       (length (assoc-ref args 'optional)))
					    (length values)))))
	  (else '()))))

(define (arguments-calling proc values)
  "Return an alist with procedure arguments and their values"
  (let ((args ((@ (ice-9 session) procedure-arguments) proc)))
    (append (required-arguments args values)
	    (optional-arguments args values)
	    (keyword-arguments args values)
	    (rest-arguments args values))))

(define (required-arguments-apply args values)
  "Return a list with required arguments for use with apply"
  (define (f vars values)
    (cond ((null? vars) '())
	  (else 
	   (cons (assoc-ref values (car vars))
		 (f (cdr vars) values)))))
  (f (assoc-ref args 'required) values))

(define (optional-arguments-apply args values)
  "Return a list with optional arguments for use with apply"
  (define (f vars values)
    (cond ((null? vars) '())
	  (else (let ((a (f (cdr vars) values))
		      (val (assoc (car vars) values)))
		  (cond ((and val (bound? (cdr val)))
			 (cons (cdr val) a))
			(else a))))))
  (f (assoc-ref args 'optional) values))

(define (keyword-arguments-apply args values)
  "Return a list with keyword arguments for use with apply"
  (define (f vars values)
    (cond ((null? vars) '())
	  (else (let ((a (f (cdr vars) values))
		      (val (assoc (keyword->symbol (car vars)) values)))
		  (cond ((and val (bound? (cdr val)))
			 (cons (car vars) (cons (cdr val) a)))
			(else a))))))
  (f (map (lambda (x) (car x)) (assoc-ref args 'keyword)) values))

(define (rest-arguments-apply args values)
  "Return a list with rest arguments for use with apply"
  (let ((rest (assoc-ref args 'rest)))
    (cond (rest (assoc-ref values rest))
	  (else '()))))
  
(define (arguments-apply proc values)
  "Return a list for use with apply"
  (let ((args ((@ (ice-9 session) procedure-arguments) proc)))
    (append (required-arguments-apply args values)
	    (optional-arguments-apply args values)
	    (keyword-arguments-apply args values)
	    (rest-arguments-apply args values))))
