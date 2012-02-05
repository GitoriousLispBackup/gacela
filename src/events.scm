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


(define-module (gacela events)
  #:use-module (gacela sdl)
  #:export (process-events
	    quit-signal?
	    key?
	    key-pressed?
	    key-released?))


(define (get-event events types)
  (cond ((null? events) '())
	(else
	 (let ((res (get-event (cdr events) types))
	       (event (car events)))
	   (cond ((member (assoc-ref event 'type) types) (cons event res))
		 (else res))))))

(define (poll-events)
  (let ((event (SDL_PollEvent)))
    (cond ((null? event) '())
	  (else (cons event (poll-events))))))

(define (process-events)
  (let ((events (poll-events)))
    (process-screen-events (get-event events `(,SDL_QUIT)))
    (process-keyboard-events (get-event events `(,SDL_KEYDOWN ,SDL_KEYUP)))))


;;; Screen Events

(define quit-signal #f)

(define (process-screen-events events)
  (set! quit-signal #f)
  (process-screen-events-recursive events))

(define (process-screen-events-recursive events)
  (cond ((not (null? events))
	 (let ((event (car events)))
	   (cond ((= (assoc-ref event 'type) SDL_QUIT) (set! quit-signal #t))))
	 (process-screen-events-recursive (cdr events)))))

(define (quit-signal?)
  quit-signal)


;;; Keyboard Events

(define keymap (make-hash-table))
(define pressed (make-hash-table))
(define released (make-hash-table))

(define (process-keyboard-events events)
  (clear-key-state)
  (process-keyboard-events-recursive events))

(define (process-keyboard-events-recursive events)
  (cond ((not (null? events))
	 (let ((event (car events)))
	   (cond ((= (assoc-ref event 'type) SDL_KEYDOWN) (key-press (assoc-ref event 'key.keysym.sym)))
		 ((= (assoc-ref event 'type) SDL_KEYUP) (key-release (assoc-ref event 'key.keysym.sym)))))
	 (process-keyboard-events-recursive (cdr events)))))

(define (key? key)
  (hash-ref keymap (get-keycode key)))

(define (key-pressed? key)
  (hash-ref pressed (get-keycode key)))

(define (key-released? key)
  (hash-ref released (get-keycode key)))

(define (key-press key-code)
  (hash-set! keymap key-code #t)
  (hash-set! pressed key-code #t)
  (hash-set! released key-code #f))

(define (key-release key-code)
  (hash-set! keymap key-code #f)
  (hash-set! pressed key-code #f)
  (hash-set! released key-code #t))

(define (clear-keymap)
  (hash-clear! keymap))

(define (clear-key-state)
  (hash-clear! pressed)
  (hash-clear! released))


(define keys
  '((8 . backspace)
    (9 . tab)
    (12 . clear)
    (13 . return)
    (19 . pause)
    (27 . escape)
    (32 . space)
    (33 . exclaim)
    (34 . quotedbl)
    (35 . hash)
    (36 . dollar)
    (38 . ampersand)
    (39 . quote)
    (40 . leftparen)
    (41 . rightparen)
    (42 . asterisk)
    (43 . plus)
    (44 . comma)
    (45 . minus)
    (46 . period)
    (47 . slash)
    (48 . 0)
    (49 . 1)
    (50 . 2)
    (51 . 3)
    (52 . 4)
    (53 . 5)
    (54 . 6)
    (55 . 7)
    (56 . 8)
    (57 . 9)
    (58 . colon)
    (59 . semicolon)
    (60 . less)
    (61 . equals)
    (62 . greater)
    (63 . question)
    (64 . at)
    (269 . kp-minus)
    (270 . kp-plus)
    (273 . up)
    (274 . down)
    (275 . right)
    (276 . left)
    (282 . f1)
    (283 . f2)
    (284 . f3)
    (285 . f4)
    (286 . f5)
    (287 . f6)
    (288 . f7)
    (289 . f8)
    (290 . f9)
    (291 . f10)
    (292 . f11)
    (293 . f12)))

(define keynames (map (lambda (k) (cons (cdr k) (car k))) keys))

(define (get-keycode keyname)
  (assoc-ref keynames keyname))

(define (get-keyname keycode)
  (assoc-ref keys keycode))
