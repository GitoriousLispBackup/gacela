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


(define-module (gacela examples engine-conway-game)
  #:use-module (gacela system)
  #:use-module (gacela engine)
  #:use-module (ice-9 receive))


(define* (neighborhood cell #:key (size 1))
  (let ((min (* -1 size)))
    (let loop-x ((delta-x size) (res '()))
      (cond ((< delta-x min) res)
	    (else
	     (loop-x (- delta-x 1)
		     (let loop-y ((delta-y size) (res res))
		       (cond ((< delta-y min) res)
			     (else
			      (loop-y (- delta-y 1)
				      (cond ((not (and (= delta-x 0) (= delta-y 0)))
					     (cons (list (+ delta-x (car cell)) (+ delta-y (cadr cell))) res))
					    (else res))))))))))))

(define* (frequencies cells #:optional (res '()))
  (cond ((null? cells)
	 res)
	(else
	 (let ((freq (or (assoc-ref res (car cells)) 0)))
	   (frequencies (cdr cells)
			(assoc-set! res (car cells) (+ freq 1)))))))

(define* (dead-loop cells freq #:optional (deads '()))
  (cond ((null? cells)
	 (values freq deads))
	(else
	 (let* ((key (get-key (car cells)))
		(coord (get-component 'coord (car cells)))
		(f (or (assoc-ref freq coord) 0))
		(new-freq (assoc-remove! freq coord)))
	   (cond ((not (or (= f 2) (= f 3)))
		  (dead-loop (cdr cells)
			     new-freq
			     (cons (remove-entity key) deads)))
		 (else
		  (dead-loop (cdr cells) new-freq deads)))))))

(define* (live-loop freq #:optional (lives '()))
  (cond ((null? freq)
	 lives)
	(else
	 (cond ((= (cdar freq) 3)
		(live-loop (cdr freq)
			   (cons (new-entity `(coord . ,(caar freq))) lives)))
	       (else
		(live-loop (cdr freq) lives))))))

(define-system lives-or-deads ((cells (coord)))
  (let ((freq (frequencies (apply append (map (lambda (c) (neighborhood (get-component 'coord c))) cells)))))
    (receive (freq2 deads) (dead-loop cells freq)
      (entities-changes
       (append deads
	       (live-loop freq2))))))

(define-system print-world ((cells (coord)))
  (format #t "Live Cells: ~a~%" (length cells)))

(define-engine conway-game lives-or-deads print-world)

(with-engine conway-game ()
  (let ((cells '((4 1) (4 2) (5 1) (5 2)
		 (11 3) (11 4) (11 5) (12 2) (12 6) (13 1) (13 7) (14 1) (14 7)
		 (15 4) (16 2) (16 6) (17 3) (17 4) (17 5) (18 4)
		 (21 5) (21 6) (21 7) (22 5) (22 6) (22 7) (23 4) (23 8)
		 (25 3) (25 4) (25 8) (25 9)
		 (35 6) (35 7) (36 6) (36 7))))
    (entities-changes
     (map (lambda (c) (new-entity `(coord . ,c))) cells))))


(export neighborhood
	frequencies
	dead-loop
	live-loop
	conway-game)
