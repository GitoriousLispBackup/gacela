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


(define-module (gacela views)
  #:use-module (gacela gacela)
  #:use-module ((gacela video) #:renamer (symbol-prefix-proc 'video:))
  #:use-module ((gacela gl) #:select (glPushMatrix glPopMatrix))
  #:use-module (ice-9 optargs))

(define-macro (define-view name content)
  `(begin
     (hash-set! active-views ',name (lambda () (video:glmatrix-block ,content)))
     ',name))

(define (mesh primitive)
  (let ((x 0) (y 0) (z 0)
	(ax 0) (ay 0) (az 0)
	(rx 0) (ry 0) (rz 0))
    (lambda (option . params)
      (case option
	((draw)
	 (video:glatrix-block
	  (video:rotate rx ry rz)
	  (video:translate x y z)
	  (video:rotate ax ay az)
	  (primitive)))
	((get-properties)
	 `((x . ,x) (y . ,y) (z . ,z) (ax . ,ax) (ay . ,ay) (az . ,az) (rx . ,rx) (ry . ,ry) (rz . ,rz)))))))

(define-macro (define-mob mob-head . body)
  (let* ((name (car mob-head))
	 (attr (cdr mob-head))
	 (make-fun-symbol (gensym))
	 (mob-fun-symbol (gensym))
	 (params-symbol (gensym)))
    `(define (,name . ,params-symbol)
       (define ,make-fun-symbol
	 (lambda* ,(if (null? attr) '() `(#:key ,@attr))
	   (the-mob ,name (list ,@(map (lambda (a) `(cons ',(car a) ,(car a))) attr)))))
       (define ,mob-fun-symbol
	 (define-mob-function ,attr ,@body))
       (cond ((or (null? ,params-symbol) (keyword? (car ,params-symbol)))
	      (apply ,make-fun-symbol ,params-symbol))
	     (else
	      (apply ,mob-fun-symbol ,params-symbol))))))


(define-macro (define-mesh name . mesh)
  (let* ((make-fun-symbol (gensym))
	 (mesh-fun-symbol (gensym))
	 (params-symbol (gensym)))
    `(define ,name
       (let ((,make-fun-symbol
	      (lambda ()))
	     (,mesh-fun-symbol
	      (lambda ())))
	 (lambda (. ,params-symbol)
	   (cond ((or (null? ,params-symbol) (keyword? (car ,params-symbol)))
		  (apply ,make-fun-symbol ,params-symbol))
		 (else
		  (apply ,mesh-fun-symbol ,params-symbol))))))))
	 

(define-macro (define-primitives . symbols)
  (cond ((null? symbols)
	 `#t)
	(else
	 `(begin
	    (define (,(caar symbols) . params) (mesh (lambda () (apply ,(cadar symbols) params))))
	    (define-primitives ,@(cdr symbols))))))

; (define-macro (,(caar symbols) . params) (let ((f ',(cadar symbols))) `(mesh (lambda () (apply ,f ',params)))))

(define-primitives
  (rectangle video:draw-rectangle)
  (square video:draw-square))


(module-map (lambda (sym var)
	      (if (not (eq? sym '%module-public-interface))
		  (module-export! (current-module) (list sym))))
	    (current-module))
