;;; Gacela, a GNU Guile extension for fast games development
;;; Copyright (C) 2013 by Javier Sancho Fernandez <jsf at jsancho dot org>
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


(define-module (gacela system)
  #:use-module (srfi srfi-9))


;;; Component definitions

(define (make-symbol . args)
  (string->symbol
   (string-concatenate
    (map (lambda (a) (if (symbol? a) (symbol->string a) a)) args))))

(define-macro (define-component name . args)
  `(begin
     (use-modules (srfi srfi-9) (srfi srfi-9 gnu))
     (define-record-type ,name
       (,(make-symbol "make-" name) ,@args)
       ,(make-symbol name "?")
       ,@(map (lambda (a) (list a (make-symbol name "-" a) (make-symbol "set-" name "-" a "!"))) args))
     (set-record-type-printer! ,name
       (lambda (record port)
	 (format port "#<[~a]" ',name)
	 ,@(map (lambda (a) `(format port " ~a: ~a" ',a (,(make-symbol name "-" a) record))) args)
	 (format port ">")))
     ',name))

(define (get-component-type component)
  (record-type-name (record-type-descriptor component)))

(export define-component
	get-component-type)
