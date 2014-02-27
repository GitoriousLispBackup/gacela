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


(define-system s1 ((with-l (l)))
  (sleep 3)
  (entities-changes
   (map (lambda (e)
	  (set-entity-components (get-key e) '(l1 . 1)))
	with-l)))

(define-system s2 ((with-l (l)))
  (sleep 4)
  (entities-changes
   (map (lambda (e)
	  (set-entity-components (get-key e) '(l2 . 2)))
	with-l)))

(define (composing-with-join)
  (let ((entities (make-entity-set (new-entity '(l . ())) (new-entity '(l . ())) (new-entity '(a . #f)))))
    (set! entities (modify-entities entities (get-entities-changes ((join-systems s1 s2) entities))))
    (entity-list entities)))

(export composing-with-join)


(define (composing-with-thread)
  (let ((entities (make-entity-set (new-entity '(l . ())) (new-entity '(l . ())) (new-entity '(a . #f)))))
    (set! entities (modify-entities entities (get-entities-changes ((thread-systems s1 s2) entities))))
    (entity-list entities)))
  
(export composing-with-thread)


(define (join-vs-thread)
  (let ((entities (make-entity-set (new-entity '(l . ())) (new-entity '(l . ())) (new-entity '(a . #f))))
	(t (current-time)))
    (set! entities (modify-entities entities (get-entities-changes ((join-systems s1 s2) entities))))
    (format #t "~a~%Time: ~a~%~%" entities (- (current-time) t)))

  (let ((entities (make-entity-set (new-entity '(l . ())) (new-entity '(l . ())) (new-entity '(a . #f))))
	(t (current-time)))
    (set! entities (modify-entities entities (get-entities-changes ((thread-systems s1 s2) entities))))
    (format #t "~a~%Time: ~a~%~%" entities (- (current-time) t))))

(export join-vs-thread)
