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


;;; World of Mob

(in-package :gacela)

(defmacro makemob (name variables &rest methods)
  `(lambda ,

(defmacro defmob (name variables &key init logic render)
  `(let ((make-name ',(intern (concatenate 'string "MAKE-" (string name)))))
     (setf (symbol-function make-name)
	   (makemob ,variables :init ,init :logic ,logic :render ,render))
     make-name))

;(defmacro makemob (variables &key init logic render)
;  `(lambda
;     ,(if (null variables) () (cons '&key variables))
;     (mob-structure ,variables ,init ,logic ,render)))

(defmacro mob-structure (variables init logic render)
  `(list
    :init (lambda () ,init)
    :logic (lambda () ,logic)
    :render (lambda () ,render)
    :context (lambda ()
	       ,(if variables
		    `(mapcar #'list
			     ',(mapcar #'car+ variables)
			     (multiple-value-list
			      (values-list ,(cons 'list (mapcar #'car+ variables)))))
		  nil))))

(defun init-mob (mob)
  (funcall (getf mob :init)))

(defun logic-mob (mob)
  (funcall (getf mob :logic)))

(defun render-mob (mob)
  (funcall (getf mob :render)))

(let (running-mobs mobs-to-add mobs-to-quit)
  (defun mob-on (mob)
    (push mob mobs-to-add))

  (defun mob-off (mob)
    (push mob mobs-to-quit)))
