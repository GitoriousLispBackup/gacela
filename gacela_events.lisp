;;; Gacela, a GNU Common Lisp extension for fast games development
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


(eval-when (compile load eval)
	   (when (not (find-package 'gacela)) (make-package 'gacela :nicknames '(gg) :use '(lisp)))
	   (in-package 'gacela :nicknames '(gg) :use '(lisp)))


;;; SDL Events
(defconstant SDL_NOEVENT          0)
(defconstant SDL_ACTIVEEVENT      1)
(defconstant SDL_KEYDOWN          2)
(defconstant SDL_KEYUP            3)
(defconstant SDL_MOUSEMOTION      4)
(defconstant SDL_MOUSEBUTTONDOWN  5)
(defconstant SDL_MOUSEBUTTONUP    6)
(defconstant SDL_JOYAXISMOTION    7)
(defconstant SDL_JOYBALLMOTION    8)
(defconstant SDL_JOYHATMOTION     9)
(defconstant SDL_JOYBUTTONDOWN    10)
(defconstant SDL_JOYBUTTONUP      11)
(defconstant SDL_QUIT             12)
(defconstant SDL_SYSWMEVENT       13)
(defconstant SDL_EVENT_RESERVEDA  14)
(defconstant SDL_EVENT_RESERVEDB  15)
(defconstant SDL_VIDEORESIZE      16)
(defconstant SDL_VIDEOEXPOSE      17)
(defconstant SDL_EVENT_RESERVED2  18)
(defconstant SDL_EVENT_RESERVED3  19)
(defconstant SDL_EVENT_RESERVED4  20)
(defconstant SDL_EVENT_RESERVED5  21)
(defconstant SDL_EVENT_RESERVED6  22)
(defconstant SDL_EVENT_RESERVED7  23)
(defconstant SDL_USEREVENT        24)
(defconstant SDL_NUMEVENTS        32)

;;; Functions
(defun get-event (events &rest types)
  (remove nil (mapcar
	       (lambda (l)
		 (cond ((member (getf l :type) types) l)))
	       events)))

(defun poll-events ()
  (let ((event (SDL_PollEvent)))
    (cond ((null event) nil)
	  (t (cons event (poll-events))))))

(defun process-events ()
  (let ((events (poll-events)))
    (quit? t (and (get-event events SDL_QUIT) t))
    (clear-key-state)
    (process-keyboard-events (get-event events SDL_KEYDOWN SDL_KEYUP))))

(let (will-happen happenings)
  (defun next-happenings ()
    (setq happenings will-happen)
    (setq will-happen nil))

  (defun will-happen (happening)
    (setq will-happen (cons happening will-happen)))

  (defun is-happening? (happening &optional (test #'eql))
    (remove nil (mapcar
		 (lambda (l)
		   (cond ((funcall test happening l) l)))
		 happenings))))

(let (quit)
  (defun quit? (&optional change newquit)
    (if change (setq quit newquit) quit)))

(defun process-keyboard-events (events)
  (cond (events
	 (let ((event (car events)))
	   (cond ((= (getf event :type) SDL_KEYDOWN) (key-press (getf event :key.keysym.sym)))
		 ((= (getf event :type) SDL_KEYUP) (key-release (getf event :key.keysym.sym)))))
	 (process-keyboard-events (cdr events)))))

(let ((keymap (make-hash-table))
      (pressed (make-hash-table))
      (released (make-hash-table)))
  (defun key? (key)
    (gethash (get-keycode key) keymap))

  (defun key-pressed? (key)
    (gethash (get-keycode key) pressed))

  (defun key-released? (key)
    (gethash (get-keycode key) released))

  (defun key-press (key-code)
    (setf (gethash key-code keymap) t)
    (setf (gethash key-code pressed) t)
    (setf (gethash key-code released) nil))

  (defun key-release (key-code)
    (setf (gethash key-code keymap) nil)
    (setf (gethash key-code pressed) nil)
    (setf (gethash key-code released) t))

  (defun clear-keymap ()
    (clrhash keymap))

  (defun clear-key-state ()
    (clrhash pressed)
    (clrhash released)))

(let ((keys
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
	 (293 . f12))))

  (defun get-keycode (keyname)
    (car (rassoc keyname keys)))

  (defun get-keyname (keycode)
    (cdr (assoc keycode keys))))
