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


;;; Engine definitions

(define-record-type engine
  (make-engine-record entities mutex system)
  engine?
  (entities engine-entities set-engine-entities!)
  (mutex engine-mutex set-engine-mutex!)
  (system engine-system set-engine-system!))

(set-record-type-printer! engine
  (lambda (record port)
    (format port "#<[engine] entities: ~a>"
	    (length (car (engine-entities record))))))

(define (make-engine . systems)
  (make-engine-record
   '(() ())
   (make-mutex)
   (if (not (= (length systems) 1))
       (join-systems systems)
       (car systems))))

(define-syntax define-engine
  (syntax-rules ()
    ((_ name system ...)
     (define name
       (make-engine system ...)))))

(export make-engine
	define-engine)


;;; Engine Access Protocol Interface

(define (with-engine engine . changes)
  (with-mutex (engine-mutex engine)
    (let ((entities (engine-entities engine)))
      (receive (e c) (modify-entities changes (car entities) (cadr entities))
        (set-engine-entities! engine (list e c))))))

(export with-engine)
