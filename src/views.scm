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

(define mesh-type
  (make-record-type "mesh" 
		    '(draw translate turn rotate inner-properties inner-property properties properties-set! property property-set!)))

(define mesh-constructor (record-constructor mesh-type))
(define mesh? (record-predicate mesh-type))

(define (mesh proc)
  (apply
   mesh-constructor
   (let ((px 0) (py 0) (pz 0)
	 (ax 0) (ay 0) (az 0)
	 (rx 0) (ry 0) (rz 0)
	 (id (gensym))
	 (properties '()))
     (let ((inner-properties
	    (lambda ()
	      `((id . ,id) (x . ,px) (y . ,py) (z . ,pz) (ax . ,ax) (ay . ,ay) (az . ,az) (rx . ,rx) (ry . ,ry) (rz . ,rz)))))
       (list
	(lambda ()
	  "draw"
	  (video:glmatrix-block
	   (video:rotate ax ay az)
	   (video:translate px py pz)
	   (video:rotate rx ry rz)
	   (proc properties)))
	(lambda (x y z)
	  "translate"
	  (set! px (+ px x))
	  (set! py (+ py y))
	  (set! pz (+ pz z)))
	(lambda (x y z)
	  "turn"
	  (set! ax (+ ax x))
	  (set! ay (+ ay y))
	  (set! az (+ az z)))
	(lambda (x y z)
	  "rotate"
	  (set! rx (+ rx x))
	  (set! ry (+ ry y))
	  (set! rz (+ rz z)))
	(lambda ()
	  "inner-properties"
	  (inner-properties))
	(lambda (prop-name)
	  "inner-property"
	  (assoc-ref (inner-properties) prop-name))
	(lambda ()
	  "properties"
	  properties)
	(lambda (new-properties)
	  "properties-set!"
	  (set! properties new-properties))
	(lambda (prop-name)
	  "property"
	  (assoc-ref properties prop-name))
	(lambda (prop-name value)
	  "property-set!"
	  (set! properties (assoc-set! properties prop-name value))))))))

(define mesh-properties-set!
  (let ((f (record-accessor mesh-type 'properties-set!)))
    (lambda (mesh new-properties)
      ((f mesh) new-properties))))

(define* (show mesh #:optional (view default-view))
  (let ((id (mesh 'inner-property 'id)))
    (if (not (hash-ref view id))
	(hash-set! view id mesh))))

(define* (hide mesh #:optional (view default-view))
  (hash-remove! view (mesh 'inner-property 'id)))

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


;;; Primitives

(define-macro (primitive proc)
  `(lambda (. params)
     (let ((m (mesh (lambda (props) (apply ,proc ((@ (gacela utils) arguments-apply) ,proc props))))))
       (mesh-properties-set! m ((@ (gacela utils) arguments-calling) ,proc params))
       m)))

(define-macro (define-primitives . symbols)
  (cond ((null? symbols)
	 `#t)
	(else
	 (let ((origin (caar symbols))
	       (dest (cadar symbols)))
	   `(begin
	      (define ,origin (primitive ,dest))
	      (define-primitives ,@(cdr symbols)))))))

(define-primitives
  (rectangle video:draw-rectangle)
  (square video:draw-square))


(module-map (lambda (sym var)
	      (if (not (eq? sym '%module-public-interface))
		  (module-export! (current-module) (list sym))))
	    (current-module))
