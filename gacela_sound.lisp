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


(defun load-sound (filename &key static)
  (let ((key (make-resource-sound :filename filename)))
    (cond ((get-resource key) key)
	  (t (true-load-sound filename static)))))

(defun true-load-sound (filename static)
  (init-audio)
  (let ((key (make-resource-sound :filename filename))
	(sound (Mix_LoadWAV filename)))
    (cond ((/= sound 0)
	   (set-resource key
			 `(:id-sound ,sound)
			 (lambda () (true-load-sound filename static))
			 (lambda () (Mix_FreeChunk sound))
			 :static static)
	   key))))

(defun play-sound (sound &optional (loops 0))
  (let ((id-sound (getf (get-resource sound) :id-sound)))
    (/= (Mix_PlayChannel -1 id-sound loops) -1)))

(defun load-music (filename &key static)
  (let ((key (make-resource-music :filename filename)))
    (cond ((get-resource key) key)
	  (t (true-load-music filename static)))))

(defun true-load-music (filename static)
  (init-audio)
  (let ((key (make-resource-music :filename filename))
	(music (Mix_LoadMUS filename)))
    (cond ((/= music 0)
	   (set-resource key
			 `(:id-music ,music)
			 (lambda () (true-load-music filename static))
			 (lambda () (Mix_FreeMusic music))
			 :static static)
	   key))))

(defun playing-music? ()
  (/= (Mix_PlayingMusic) 0))

(defun play-music (music &optional (loops -1))
  (cond ((not (playing-music?))
	 (let ((id-music (getf (get-resource music) :id-music)))
	   (/= (Mix_PlayMusic id-music loops) -1)))))
