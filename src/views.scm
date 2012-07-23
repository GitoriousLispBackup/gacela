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
	(rx 0) (ry 0) (rz 0)
	(id (gensym))
	(properties '()))
    (let ((inner-properties
	   (lambda ()
	     `((id . ,id) (x . ,x) (y . ,y) (z . ,z) (ax . ,ax) (ay . ,ay) (az . ,az) (rx . ,rx) (ry . ,ry) (rz . ,rz)))))
      (lambda (option . params)
	(case option
	  ((draw)
	   (video:glmatrix-block
	    (video:rotate ax ay az)
	    (video:translate x y z)
	    (video:rotate rx ry rz)
	    (primitive)))
	  ((translate)
	   (set! x (+ x (car params)))
	   (set! y (+ y (cadr params)))
	   (set! z (+ z (caddr params))))
	  ((turn)
	   (set! ax (+ ax (car params)))
	   (set! ay (+ ay (cadr params)))
	   (set! az (+ az (caddr params))))
	  ((rotate)
	   (set! rx (+ rx (car params)))
	   (set! ry (+ ry (cadr params)))
	   (set! rz (+ rz (caddr params))))
	  ((inner-properties)
	   (inner-properties))
	  ((inner-property)
	   (assoc-ref (inner-properties) (car params)))
	  ((properties)
	   properties)
	  ((property)
	   (assoc-ref properties (car params)))
	  ((property-set!)
	   (set! properties (assoc-set! properties (car params) (cadr params)))))))))

(define* (show mesh #:optional (view default-view))
  (let ((id (mesh 'get-property 'id)))
    (if (not (hash-ref view id))
	(hash-set! view id mesh))))

(define* (hide mesh #:optional (view default-view))
  (hash-remove! view (mesh 'get-property 'id)))

(define* (translate mesh x y #:optional (z 0))
  (mesh 'translate x y z)
  mesh)

(define (turn mesh . params)
  (if (>= (length params) 3)
      (apply mesh (cons 'turn params))
      (mesh 'turn 0 0 (car params)))
  mesh)

(define (rotate mesh . params)
  (if (>= (length params) 3)
      (apply mesh (cons 'rotate params))
      (mesh 'rotate 0 0 (car params)))
  mesh)

(define-macro (define-primitives . symbols)
  (cond ((null? symbols)
	 `#t)
	(else
	 (let ((origin (caar symbols))
	       (dest (cadar symbols)))
	   `(begin
	      ,(if (and (list? origin) (list? dest))
		   `(define* ,origin #f)
		   `(define (,origin . params) (mesh (lambda () (apply ,dest params)))))
	      (define-primitives ,@(cdr symbols)))))))

(define-primitives
  (rectangle video:draw-rectangle)
  (square video:draw-square))


(module-map (lambda (sym var)
	      (if (not (eq? sym '%module-public-interface))
		  (module-export! (current-module) (list sym))))
	    (current-module))
