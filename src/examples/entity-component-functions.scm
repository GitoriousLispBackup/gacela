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


(define-module (gacela examples entity-component-functions)
  #:use-module (gacela system)
  #:use-module (ice-9 receive))


(define-component a x y)
(define-component b)

(define (entity-component-functions)
  (let ((entities (make-entity-set))
	(key #f))
    (receive (e k) ((new-entity (make-a 1 2) (make-b)) entities)
      (set! entities e)
      (set! key (car k)))
    (format #t "New entity with a and b:~%~a~%~%" (entity-list entities))

    (receive (e k) ((new-entity (make-a 10 20)) entities)
      (set! entities e))
    (format #t "New entity with a:~%~a~%~%" (entity-list entities))

    (set! entities (modify-entities entities (list (set-entity-components key (make-a 50 50)) (remove-entity-components key 'b))))
    (format #t "First entity removes b and changes a:~%~a~%~%" (entity-list entities))

    (set! entities ((remove-entity key) entities))
    (format #t "Removes first entity:~%~a~%~%" (entity-list entities))

    (receive (e k) ((new-entity (make-a 1 2) (make-b)) entities)
      (set! entities e)
      (set! key (car k)))
    (format #t "New entity with a and b:~%~a~%~%" (entity-list entities))

    (set! entities (modify-entities entities (list (set-entity-components key (make-a 50 50)) (remove-entity-components key 'b) (new-entity (make-a 1000 1000)))))
    (format #t "Last entity removes b and changes a, and new entity with a:~%~a~%~%" (entity-list entities))

    (set! entities (modify-entities entities (list (remove-entity key))))
    (format #t "Remove last entity:~%~a~%~%" (entity-list entities))))

(export entity-component-functions)
