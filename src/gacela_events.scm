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
    (quit? (not (null? (get-event events `(,SDL_QUIT)))))
    (clear-key-state)
    (process-keyboard-events (get-event events `(,SDL_KEYDOWN ,SDL_KEYUP)))))

(define quit? #f)

(let ((quit #f))
  (set! quit?
	(lambda* (#:optional (value '()))
	  (if (null? value) quit (set! quit value)))))

(define (process-keyboard-events events)
  (cond ((not (null? events))
	 (let ((event (car events)))
	   (cond ((= (assoc-ref event 'type) SDL_KEYDOWN) (key-press (assoc-ref event 'key.keysym.sym)))
		 ((= (assoc-ref event 'type) SDL_KEYUP) (key-release (assoc-ref event :key.keysym.sym)))))
	 (process-keyboard-events (cdr events)))))

(define key? #f)
(define key-pressed? #f)
(define key-released? #f)
(define key-press #f)
(define key-release #f)
(define clear-keymap #f)
(define clear-key-state #f)

(let ((keymap (make-hash-table))
      (pressed (make-hash-table))
      (released (make-hash-table)))
  (set! key?
	(lambda (key)
	  (hash-ref keymap (get-keycode key))))

  (set! key-pressed?
	(lambda (key)
	  (hash-ref pressed (get-keycode key))))

  (set! key-released?
	(lambda (key)
	  (hash-ref released (get-keycode key))))

  (set! key-press
	(lambda (key-code)
	  (hash-set! keymap key-code #t)
	  (hash-set! pressed key-code #t)
	  (hash-set! released key-code #f)))

  (set! key-release
	(lambda (key-code)
	  (hash-set! keymap key-code #f)
	  (hash-set! pressed key-code #f)
	  (hash-set! released key-code #t)))

  (set! clear-keymap
	(lambda ()
	  (hash-clear! keymap)))

  (set! clear-key-state
	(lambda ()
	  (hash-clear! pressed)
	  (hash-clear! released))))

(define get-keycode #f)
(define get-keyname #f)

(let* ((keys
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
       (keynames (map (lambda (k) (cons (cdr k) (car k))) keys)))

  (set! get-keycode
	(lambda (keyname)
	  (assoc-ref keynames keyname)))

  (set! get-keyname
	(lambda (keycode)
	  (assoc-ref keys keycode))))
