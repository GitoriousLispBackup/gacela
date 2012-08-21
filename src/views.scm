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


;;; Views

(define view-type
  (make-record-type "view" 
		    '(id controllers meshes)
		    (lambda (record port)
		      (format port "#<view: ~a controllers / ~a meshes>"
			      (length (hash-map->list (lambda x x) (view-controllers record)))
			      (length (hash-map->list (lambda x x) (view-meshes record)))))))

(define view? (record-predicate view-type))
(define view-id (record-accessor view-type 'id))
(define view-controllers (record-accessor view-type 'controllers))
(define view-meshes (record-accessor view-type 'meshes))
(define (make-view) ((record-constructor view-type) (gensym) (make-hash-table) (make-hash-table)))


(define active-views (make-hash-table))

(define default-view (make-hash-table))

(define* (draw-meshes #:optional (meshes (hash-map->list (lambda (k v) v) default-view)))
  (cond ((not (null? meshes))
	 (catch #t
		  (lambda () (mesh-draw (car meshes)))
		  (lambda (key . args) #f))
	 (draw-meshes (cdr meshes)))))


;;; Meshes

(define mesh-type
  (make-record-type "mesh" 
		    '(draw translate turn rotate inner-properties inner-property properties properties-set! property property-set!)
		    (lambda (record port)
		      (format port "#<mesh: ~a" (mesh-inner-property record 'type))
		      (for-each (lambda (x)
				  (cond (((@ (gacela utils) bound?) (cdr x))
					 (format port " ~a" x))))
				(mesh-properties record))
		      (display ">" port))))
		      

(define mesh? (record-predicate mesh-type))

(define* (make-mesh proc #:optional mesh-type)
  (apply
   (record-constructor mesh-type)
   (let ((px 0) (py 0) (pz 0)
	 (ax 0) (ay 0) (az 0)
	 (rx 0) (ry 0) (rz 0)
	 (id (gensym)) (type mesh-type)
	 (properties '()))
     (let ((inner-properties
	    (lambda ()
	      `((id . ,id) (type . ,type) (x . ,px) (y . ,py) (z . ,pz) (ax . ,ax) (ay . ,ay) (az . ,az) (rx . ,rx) (ry . ,ry) (rz . ,rz)))))
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

(define (mesh-draw mesh)
  (((record-accessor mesh-type 'draw) mesh)))

(define (mesh-inner-properties mesh)
  (((record-accessor mesh-type 'inner-properties) mesh)))

(define (mesh-inner-property mesh prop-name)
  (((record-accessor mesh-type 'inner-property) mesh) prop-name))

(define (mesh-properties mesh)
  (((record-accessor mesh-type 'properties) mesh)))

(define (mesh-properties-set! mesh new-properties)
  (((record-accessor mesh-type 'properties-set!) mesh) new-properties))

(define (mesh-property mesh prop-name)
  (((record-accessor mesh-type 'property) mesh) prop-name))

(define (mesh-property-set! mesh prop-name value)
  (((record-accessor mesh-type 'property-set!) mesh) prop-name value))

(define* (show mesh #:optional (view default-view))
  (let ((id (mesh-inner-property mesh 'id)))
    (if (not (hash-ref view id))
	(hash-set! view id mesh))))

(define* (hide mesh #:optional (view default-view))
  (hash-remove! view (mesh-inner-property mesh 'id)))

(define* (translate mesh x y #:optional (z 0))
  (((record-accessor mesh-type 'translate) mesh) x y z)
  mesh)

(define (turn mesh . params)
  (apply ((record-accessor mesh-type 'turn) mesh)
	 (if (>= (length params) 3)
	     params
	     (list 0 0 (car params))))
  mesh)

(define (rotate mesh . params)
  (apply ((record-accessor mesh-type 'rotate) mesh)
	 (if (>= (length params) 3)
	     params
	     (list 0 0 (car params))))
  mesh)


;;; Primitives

(defmacro* define-primitive (proc #:optional type)
  `(lambda (. params)
     (let ((m (make-mesh (lambda (props) (apply ,proc ((@ (gacela utils) arguments-apply) ,proc props))) ,type)))
       (mesh-properties-set! m ((@ (gacela utils) arguments-calling) ,proc params))
       m)))

(define-macro (define-primitives . symbols)
  (cond ((null? symbols)
	 `#t)
	(else
	 (let ((origin (caar symbols))
	       (dest (cadar symbols)))
	   `(begin
	      (define ,origin (define-primitive ,dest ',origin))
	      (define-primitives ,@(cdr symbols)))))))

(define-primitives
  (rectangle video:draw-rectangle)
  (square video:draw-square))


(module-map (lambda (sym var)
	      (if (not (eq? sym '%module-public-interface))
		  (module-export! (current-module) (list sym))))
	    (current-module))


;;; Adding extensions to the main loop
(add-extension! draw-meshes 50)
