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

(defmacro makemob (name &rest methods)
  `(defun ,name (&rest args &aux (option (car args)))
     ,(union
       `(case option
	      (:on (mob-on #',name))
	      (:off (mob-off #',name)))
       (labels ((options (m &aux (option (car m)) (body (cadr m)))
			 (cond ((null m) nil)
			       (t (cons (list option `(apply ,body (cdr args))) (options (cddr m)))))))
	       (options methods)))))

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

(let (running-mobs mobs-to-add mobs-to-quit)
  (defun mob-on (mob)
    (push mob mobs-to-add))

  (defun logic-mobs ()
    (dolist (mob running-mobs) (funcall mob :logic)))

  (defun render-mobs ()
    (dolist (mob running-mobs) (funcall mob :render)))

  (defun mob-off (mob)
    (push mob mobs-to-quit))

  (defun refresh-running-mobs ()
    (do ((mob (pop mobs-to-add) (pop mobs-to-add))) ((null mob))
	(push mob running-mobs)
	(funcall mob :init))
    (setq running-mobs (reverse (set-difference running-mobs mobs-to-quit :test #'equal))))

  (defun quit-all-mobs ()
    (setq running-mobs nil mobs-to-add nil mobs-to-quit nil)))
