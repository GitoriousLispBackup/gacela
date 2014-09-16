;;; Gacela, a GNU Guile extension for fast games development
;;; Copyright (C) 2014 by Javier Sancho Fernandez <jsf at jsancho dot org>
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


(define-module (gacela game)
  #:use-module (gacela engine)
  #:use-module (ice-9 vlist)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu))


;;; Working with entities

(define-record-type entity-type
  (make-entity-record id components)
  entity?
  (id entity-id)
  (game entity-game set-entity-game!)
  (components entity-components set-entity-components!))

(set-record-type-printer! entity
  (lambda (record port)
    (format port "#<[entity ~a] ~a>"
	    (entity-id record)
	    (entity-components record))))

(define (entity . components)
  (make-entity-record
   (gensym)
   #f
   components))

(export entity
	entity?
	entity-id)


;;; Game Definition

(define-record-type game-type
  (make-game-record name entities)
  game?
  (name game-name set-game-name!)
  (entities game-entities set-game-entities!))

(set-record-type-printer! game
  (lambda (record port)
    (format port "#<[Game: ~a] ~a>"
	    (game-name record)
	    (map
	     (lambda (id)
	       (cdr (vhash-assoc id (game-entities record))))
	     (vhash-fold
	      (lambda (key value result)
		(lset-union eqv? (list key) result))
	      '()
	      (game-entities record))))))

(define (game name . entities)
  (make-game-record
   name
   (alist->vhash
    (map (lambda (e) (cons (entity-id e) e))
	 entities))))

(export game
	game?)


;;; Working with games

(define (add-entity game entity)
  #f)
