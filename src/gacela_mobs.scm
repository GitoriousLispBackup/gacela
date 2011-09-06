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


;;; Mobs Factory

(define show-mob-hash #f)
(define hide-mob-hash #f)
(define get-active-mobs #f)
(define clear-active-mobs #f)
(define mobs-changed? #f)

(let ((active-mobs (make-hash-table)) (changed #f))
  (set! show-mob-hash
	(lambda (key mob)
	  (hash-set! active-mobs key mob)
	  (set! changed #t)))

  (set! hide-mob-hash
	(lambda (key)
	  (hash-remove! key)
	  (set! changed #t)))

  (set! get-active-mobs
	(lambda* (#:optional (refreshed #t))
	  (set! changed (not refreshed))
	  (hash-map->list (lambda (k v) v) active-mobs)))

  (set! clear-active-mobs
	(lambda ()
	  (set! changed #t)
	  (hash-clear! active-mobs)))

  (set! mobs-changed?
	(lambda () changed)))


(define-macro (show-mob mob)
  `(show-mob-hash ',mob (lambda (option) (,mob option))))

(define-macro (hide-mob mob)
  `(hide-mob-hash ',mob))

(define (run-mobs-logic mobs)
  (for-each (lambda (m) (m 'run-logic)) mobs))

(define (render-mobs mobs)
  (for-each (lambda (m) (m 'render)) mobs))


;;; Logics and looks for mobs

(define (get-attr list name default)
  (let ((value (assoc-ref list name)))
    (cond (value (car value))
	  (else default))))

(define (attr-def attr)
  (let ((name (car attr))
	(value (cadr attr)))
    `(,name (get-attr attributes ',name ',value))))

(define (attr-save attr)
  (let ((name (car attr)))
    `(set! attributes (assoc-set! attributes ',name (list ,name)))))

(define-macro (define-mob-logic logic-head . code)
  (let ((name (car logic-head)) (attr (cdr logic-head)))
    `(define ,name
       (lambda-mob-logic ,attr ,@code))))

(define-macro (lambda-mob-logic attr . code)
  `(lambda (attributes)
     (let ,(map attr-def attr)
       ,@code
       ,(cons 'begin (map attr-save attr))
       attributes)))

(define-macro (define-mob-look look-head . code)
  (let ((name (car look-head)) (attr (cdr look-head)))
    `(define ,name
       (lambda-mob-look ,attr ,@code))))

(define-macro (lambda-mob-look attr . look)
  (define (process-look look)
    (cond ((null? look) (values '() '()))
	  (else
	   (let ((line (car look)))
	     (receive (lines images) (process-look (cdr look))
		      (cond ((string? line)
			     (let ((var (gensym)))
			       (values (cons `(draw-texture ,var) lines)
				       (cons `(,var (load-texture ,line)) images))))
			    (else
			     (values (cons line lines)
				     images))))))))

  (receive (look-lines look-images) (process-look look)
	   `(let ,look-images
		(lambda (attributes)
		  (let ,(map attr-def attr)
		    (glPushMatrix)
		    ,@look-lines
		    (glPopMatrix))))))


;;; Making mobs

(define-macro (define-mob mob-head . look)
  (let ((name (car mob-head)) (attr (cdr mob-head)))
    `(define ,name
       (lambda-mob ,attr ,@look))))

(define-macro (lambda-mob attr . look)
  `(let ((mob #f))
     (set! mob
	   (let ((attr ',attr) (actions '()) (looks '()))
	     (lambda (option . params)
	       (case option
		 ((get-attr)
		  attr)
		 ((set-attr)
		  (if (not (null? params)) (set! attr (car params))))
		 ((get-actions)
		  actions)
		 ((set-actions)
		  (if (not (null? params)) (set! actions (car params))))
		 ((get-looks)
		  looks)
		 ((set-looks)
		  (if (not (null? params)) (set! looks (car params))))
		 ((run-actions)
		  (for-each
		   (lambda (action)
		     (set! attr ((cdr action) attr)))
		   actions))
		 ((render)
		  (for-each
		   (lambda (look)
		     ((cdr look) attr))
		   looks))))))
     (cond ((not (null? ',look))
	    (mob 'set-looks
		 (list (cons
			'default-look
			(lambda-look ,attr ,@look))))))
     mob))

(define-macro (define-mob mob-def)
  (let ((name (car mob-def)) (def (cdr mob-def)))
    `(define ,name
       (lambda-mob ,@def))))

(defmacro* lambda-mob (#:key (attr '(quote ())) (logic #f) (look #f))
  `(let ((attr ,attr) (logic ,logic) (look ,look))
     (lambda (option . params)
       (case option
	 ((get-attr)
	  attr)
	 ((set-attr)
	  (if (not (null? params)) (set! attr (car params))))
	 ((get-logic)
	  logic)
	 ((set-logic)
	  (if (not (null? params)) (set! logic (car params))))
	 ((get-look)
	  look)
	 ((set-look)
	  (if (not (null? params)) (set! look (car params))))
	 ((run-logic)
	  (cond (logic
		 (catch #t
			(lambda () (set! attr (logic attr)))
			(lambda (key . args) #f)))))
	 ((render)
	  (cond (look
		 (catch #t
			(lambda () (look attr))
			(lambda (key . args) #f)))))))))

(define-macro (define-mob mob-head . body)
  (let ((name (car mob-head)) (attr (cdr mob-head)))
    `(define ,(string->symbol (string-concatenate (list "make-" (symbol->string name))))
       (lambda-mob ,attr ,@body))))

(define-macro (lambda-mob attr . body)
  `(lambda ()
     (let ,(cons '(mob-id (gentemp)) attr)
       (lambda ()
	 ,@body))))


(define (get-mob-attr mob var)
  (let ((value (assoc-ref (mob 'get-attr) var)))
    (if value (car value) #f)))

(define (set-mob-attr! mob var value)
  (mob 'set-attr (assoc-set! (mob 'get-attr) var (list value))))

(define (set-mob-logic! mob logic)
  (mob 'set-logic logic))

(define (set-mob-look! mob look)
  (mob 'set-look look))
