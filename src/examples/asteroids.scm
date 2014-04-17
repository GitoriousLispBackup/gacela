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


(define-module (gacela examples asteroids)
  #:use-module (gacela game))


(define asteroids (make-game "Asteroids"))
(define player (make-entity))
(add-component player (make-transform))
(add-component player (make-mesh))
(add-entity asteroids player)

(define asteroids
  (make-game "Asteroids"
   (make-entity
    (make-transform)
    (make-mesh))))
