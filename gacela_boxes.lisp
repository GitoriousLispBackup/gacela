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


;;; Boxes Factory

(let (visible-boxes boxes-to-add boxes-to-quit)
  (defun add-box (box)
    (push box boxes-to-add))

  (defun quit-box (box)
    (push box boxes-to-quit))

  (defun quit-all-boxes ()
    (setq visible-boxes nil boxes-to-add nil boxes-to-quit nil))

  (defun refresh-visible-boxes ()
    (cond (boxes-to-add
	   (setq visible-boxes (union visible-boxes boxes-to-add))
	   (setq boxes-to-add nil)))
    (cond (boxes-to-quit
	   (setq visible-boxes (reverse (set-difference visible-boxes boxes-to-quit)))
	   (setq boxes-to-quit nil))))

  (defun render-boxes ()
    (labels ((render (l)
		     (cond (l (funcall (render-fun-name (car l)))
			      (render (cdr l))))))
	    (render visible-boxes))))


(defun render-fun-name (name)
  (intern (concatenate 'string "RENDER-BOX-" (string name)) 'gacela))

(defun get-props-fun-name (name)
  (intern (concatenate 'string "GET-PROPERTIES-BOX-" (string name)) 'gacela))

(defmacro make-box (name properties &rest code)
  `(progn
     (let ,(union '((rx 0) (ry 0) (rz 0)) properties)
       (defun ,(render-fun-name name) () ,@code)
       (defun ,(get-props-fun-name name) () (list :rx rx :ry ry :rz rz)))
     (add-box ',name)))


;;; Translations and rotations

(defun rotate-box (&rest rot)
  )