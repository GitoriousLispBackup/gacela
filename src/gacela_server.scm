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


(define start-server #f)
(define check-connections #f)
(define stop-server #f)

(let ((server-socket #f) (clients '()))
  (set! start-server
	(lambda (port)
	  (set! server-socket (socket PF_INET SOCK_STREAM 0))
	  (fcntl server-socket F_SETFL (logior O_NONBLOCK (fcntl server-socket F_GETFL)))
	  (setsockopt server-socket SOL_SOCKET SO_REUSEADDR 1)
	  (bind server-socket AF_INET INADDR_ANY port)
	  (listen server-socket 5)))

  (set! check-connections
	(lambda ()
	  (catch #t
		 (lambda () (set! clients (cons (accept server-socket) clients)))
		 (lambda (key . args) #f))))

  (set! stop-server
	(lambda ()
	  (cond (server-socket (close server-socket) (set! server-socket #f)))