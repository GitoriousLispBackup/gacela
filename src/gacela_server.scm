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
(define clean-closed-connections #f)
(define eval-from-clients #f)
(define stop-server #f)

(let ((server-socket #f) (clients '())
      (server-pipes #f))
  (set! start-server
	(lambda* (#:key (port #f) (pipes #f))
	  (cond (port
		 (set! server-socket (socket PF_INET SOCK_STREAM 0))
		 (setsockopt server-socket SOL_SOCKET SO_REUSEADDR 1)
		 (bind server-socket AF_INET INADDR_ANY port)
		 (listen server-socket 5))
		(pipes
		 (set! server-pipes pipes)))
	  (cond ((not (game-running?))
		 (game-loop)))))


  (set! clean-closed-connections
	(lambda (conns)
	  (cond ((null? conns) '())
		(else
		 (let* ((cli (car conns)) (sock (car cli)))
		   (cond ((port-closed? sock)
			  (clean-closed-connections (cdr conns)))
			 (else
			  (cons cli (clean-closed-connections (cdr conns))))))))))

  (set! check-connections
	(lambda ()
	  (set! clients (clean-closed-connections clients))
	  (catch #t
		 (lambda ()
		   (cond ((char-ready? server-socket)
			  (set! clients (cons (accept server-socket) clients)))))
		 (lambda (key . args) #f))))

  (set! eval-from-clients
	(lambda ()
	  (cond (server-pipes
		 (eval-from-client (car server-pipes) (cdr server-pipes))))
	  (for-each
	   (lambda (cli) (eval-from-client (car cli) (car cli)))
	   clients)))

  (set! stop-server
	(lambda ()
	  (cond (server-socket
		 (close server-socket)
		 (set! server-socket #f)))
	  (for-each (lambda (cli) (close (car cli))) clients)
	  (set! clients '()))))

(define (eval-from-client rec-channel send-channel)
  (cond ((char-ready? rec-channel)
	 (catch #t
		(lambda ()
		  (let ((exp (read rec-channel)))
		    (cond ((eof-object? exp)
			   (close rec-channel))
			  (else
			   (write (format #f "~a" (eval-string exp)) send-channel)))))
		(lambda (key . args)
		  (let ((fmt (string-concatenate (list (cadr args) "~%")))
			(params (caddr args)))
		    (write
		     (if params
			 (apply format (cons #f (cons fmt params)))
			 (format #f fmt))
		     send-channel))))
	 (force-output send-channel))))


(define connect-to-server #f)

(define (connect-to-server client-socket hostname port)
  (connect client-socket AF_INET (car (hostent:addr-list (gethost hostname))) port))
