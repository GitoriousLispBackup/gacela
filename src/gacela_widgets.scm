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


;;; Timers

(define (make-timer)
  '((start . 0) (paused . 0) (state . stopped)))

(define (start-timer timer)
  (assoc-set! timer 'start (SDL_GetTicks))
  (assoc-set! timer state 'running))

(define (stop-timer timer)
  (assoc-set! timer 'state 'stopped))

(define (get-time timer)
  (cond ((eq? (assoc 'state timer) 'stopped) 0)
        ((eq? (assoc 'state timer) 'paused) (assoc 'paused timer))
        (else (- (SDL_GetTicks) (assoc 'start timer)))))

(define (pause-timer timer)
  (cond ((eq? (assoc 'state timer) 'running)
         (assoc-set! timer 'paused (- (SDL_GetTicks) (assoc 'start timer)))
         (assoc-set! timer 'state 'paused))))

(define (resume-timer timer)
  (cond ((eq? (assoc 'state timer) 'paused)
         (assoc-set! timer 'start (- (SDL_GetTicks) (assoc 'paused timer)))
         (assoc-set! timer 'state 'running))))
