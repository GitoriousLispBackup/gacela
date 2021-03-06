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


(define-module (gacela examples asteroids))
;  #:use-module (gacela game))


(define asteroids (game "Asteroids"))
(define player (entity))
(set! player (add-component player (name "player")))
(set! player (add-component player (transform)))
(set! player (add-component player (mesh)))
(set! asteroids (add-entity asteroids player))
(define rock (entity))
(set! rock (add-component rock (name "rock")))
(set! rock (add-component rock (transform)))
(set! rock (add-component rock (mesh)))
(set! asteroids (add-entity asteroids rock))


(define asteroids
  (game "Asteroids"
   (entity
    (name "player")
    (transform)
    (mesh))
   (entity
    (name "rock")
    (transform)
    (mesh))))
