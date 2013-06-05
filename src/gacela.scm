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


(define-module (gacela gacela)
  #:use-module (gacela system)
  #:use-module (ice-9 threads)
  #:use-module (srfi srfi-1))


;;; Entities and components

(define entities-mutex (make-mutex))
(define game-entities '())
(define game-components '())


(define (entity . components)
  (with-mutex entities-mutex
   (let ((key (gensym)))
     (set! game-entities
	   (acons key
		  (map (lambda (c) (list (get-component-type c) c)) components)
		  game-entities))
     (set! game-components (register-components key components))
     key)))


(define* (register-components entity components #:optional (clist game-components))
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


(define (get-entity key)
  (with-mutex entities-mutex
   (assoc key game-entities)))


(export entity
	get-entity)
