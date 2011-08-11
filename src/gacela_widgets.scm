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
  (let ((start 0) (paused 0) (state 'stopped))
    (lambda (op)
      (cond ((eq? op 'start-timer)
	     (set! start (SDL_GetTicks))
	     (set! state 'running))

	    ((eq? op 'stop-timer)
	     (set! state 'stopped))

	    ((eq? op 'get-time)
	     (cond ((eq? state 'stopped) 0)
		   ((eq? state 'paused) paused)
		   (else (- (SDL_GetTicks) start))))

	    ((eq? op 'get-state)
	     state)

	    ((eq? op 'pause-timer)
	     (cond ((eq? state 'running)
		    (set! paused (- (SDL_GetTicks) start))
		    (set! state 'paused))))

	    ((eq? op 'resume-timer)
	     (cond ((eq? state 'paused)
		    (set! start (- (SDL_GetTicks) paused))
		    (set! state 'running))))))))

(define (start-timer timer)
  (timer 'start-timer))

(define (stop-timer timer)
  (timer 'stop-timer))

(define (get-time timer)
  (timer 'get-time))

(define (get-state timer)
  (timer 'get-state))

(define (pause-timer timer)
  (timer 'pause-timer))

(define (resume-timer timer)
  (timer 'resume-timer))
