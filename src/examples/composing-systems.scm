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


(define-module (gacela examples composing-systems)
  #:use-module (gacela system)
  #:use-module (ice-9 receive))


(define-system (s1 l)
  (lambda (e)
    (sleep 3)
    (map
     (lambda (e1)
       (set-entity-components (car e1) `(l . ,(cons 1 (cdadr e1)))))
     e)))

(define-system (s2 l)
  (lambda (e)
    (sleep 4)
    (map
     (lambda (e1)
       (set-entity-components (car e1) `(l . ,(cons 2 (cdadr e1)))))
     e)))

(define (composing-with-join)
  (let ((entities '())
	(components '()))
    (receive (e c) (modify-entities (list (new-entity '(l . ())) (new-entity '(l . ()))) entities components)
      ((join-systems s1 s2) e c))))

(export composing-with-join)


(define (composing-with-threaded)
  (let ((entities '())
	(components '()))
    (receive (e c) (modify-entities (list (new-entity '(l . ())) (new-entity '(l . ()))) entities components)
      ((threaded-systems s1 s2) e c))))

(export composing-with-threaded)


(define (join-vs-threaded)
  (let ((entities '())
	(components '())
	(t (current-time)))
    (receive (e c) (modify-entities (list (new-entity '(l . ())) (new-entity '(l . ()))) entities components)
      (receive (e c) ((join-systems s1 s2) e c)
	(format #t "~a~%~a~%Time: ~a~%~%" e c (- (current-time) t)))))

  (let ((entities '())
	(components '())
	(t (current-time)))
    (receive (e c) (modify-entities (list (new-entity '(l . ())) (new-entity '(l . ()))) entities components)
      (receive (e c) ((threaded-systems s1 s2) e c)
	(format #t "~a~%~a~%Time: ~a~%~%" e c (- (current-time) t))))))

(export join-vs-threaded)
