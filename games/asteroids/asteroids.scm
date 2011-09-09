(set-game-properties! #:title "Gacela Asteroids")

(define max-x (/ (assoc-ref (get-game-properties) 'width) 2))
(define min-x (- max-x))
(define max-y (/ (assoc-ref (get-game-properties) 'height) 2))
(define min-y (- max-y))

(define-mob (asteroid
	     (image (load-texture "Asteroid.png"))
	     (x 0) (y 0) (angle 0))
  (translate x y)
  (rotate angle)
  (draw-texture image))

(show-mob (make-asteroid))

;; (define draw-asteroid
;;   (let ((asteroid (load-texture "Asteroid.png")))
;;     (lambda (a)
;;       (to-origin)
;;       (translate (assoc-ref a 'x) (assoc-ref a 'y))
;;       (rotate (assoc-ref a 'angle))
;;       (draw-texture asteroid))))

;; (define (move-asteroid a)
;;   (let ((x (assoc-ref a 'x)) (y (assoc-ref a 'y))
;; 	(angle (assoc-ref a 'angle))
;; 	(vx (assoc-ref a 'vx)) (vy (assoc-ref a 'vy)))
;;     (set! x (+ x vx))
;;     (set! y (+ y vy))
;;     (cond ((> x max-x) (set! vx -1))
;; 	  ((< x min-x) (set! vx 1)))
;;     (cond ((> y max-y) (set! vy -1))
;; 	  ((< y min-y) (set! vy 1)))

;;     (assoc-multiple-set! a 'x x 'y y 'angle (+ angle 1) 'vx vx 'vy vy)))

;; (define draw-ship
;;   (let ((ship1 (load-texture "Ship1.png"))
;; 	(ship2 (load-texture "Ship2.png")))
;;     (lambda (s)
;;       (to-origin)
;;       (translate (assoc-ref s 'x) (assoc-ref s 'y))
;;       (rotate (assoc-ref s 'angle))
;;       (let ((ship (if (assoc-ref s 'moving) ship2 ship1)))
;; 	(draw-texture ship)))))

;; (define (move-ship ship)
;;   (let* ((s ship)
;; 	 (x (assoc-ref s 'x)) (y (assoc-ref s 'y))
;; 	 (angle (assoc-ref s 'angle))
;; 	 (moving (assoc-ref s 'moving)))
;;     (cond ((key? 'left) (set! angle (+ angle 5)))
;; 	  ((key? 'right) (set! angle (- angle 5))))
;;     (cond ((key? 'up)
;; 	   (let ((r (degrees-to-radians (- angle))))
;; 	     (set! x (+ x (* 4 (sin r))))
;; 	     (set! y (+ y (* 4 (cos r)))))
;; 	   (cond ((> x max-x) (set! x min-x))
;; 		 ((< x min-x) (set! x max-x)))
;; 	   (cond ((> y max-y) (set! y min-y))
;; 		 ((< y min-y) (set! y max-y)))
;; 	   (set! moving #t))
;; 	  (else
;; 	   (set! moving #f)))

;;     (assoc-multiple-set! s 'x x 'y y 'angle angle 'moving moving)))

;; (define (ship-shot s)
;;   (cond ((key-pressed? 'space)
;; 	 `((x . ,(assoc-ref s 'x)) (y . ,(assoc-ref s 'y)) (angle . ,(assoc-ref s 'angle))))
;; 	(else
;; 	 #f)))

;; (define (draw-shot sh)
;;   (to-origin)
;;   (translate (assoc-ref sh 'x) (assoc-ref sh 'y))
;;   (rotate (assoc-ref sh 'angle))
;;   (draw-line 10))

;; (define (move-shots shots)
;;   (cond ((null? shots) '())
;; 	(else
;; 	 (let* ((sh (car shots))
;; 		(x (assoc-ref sh 'x)) (y (assoc-ref sh 'y))
;; 		(angle (assoc-ref sh 'angle))
;; 		(r (degrees-to-radians (- angle))))
;; 	   (set! x (+ x (* 10 (sin r))))
;; 	   (set! y (+ y (* 10 (cos r))))
;; 	   (cond ((and (<= x max-x)
;; 		       (>= x min-x)
;; 		       (<= y max-y)
;; 		       (>= y min-y))
;; 		  (cons `((x . ,x) (y . ,y) (angle . ,angle))
;; 			(move-shots (cdr shots))))
;; 		 (else
;; 		  (move-shots (cdr shots))))))))
  
;; (define (make-asteroids n)
;;   (define (xy n r)
;;     (let ((n2 (- (random (* n 2)) n)))
;;       (cond ((and (< n2 r) (>= n2 0)) r)
;; 	    ((and (> n2 (- r)) (< n2 0)) (- r))
;; 	    (else n2))))

;;   (cond ((= n 0) '())
;; 	(else
;; 	 (cons `((x . ,(xy max-x 20)) (y . ,(xy max-y 20)) (angle . 0) (vx . 1) (vy . 1) (size . 95))
;; 	       (make-asteroids (- n 1))))))

;; (define (killed-ship? s a)
;;   (cond ((null? a) #f)
;; 	(else
;; 	 (or (< (distance-between-points (list (assoc-ref s 'x) (assoc-ref s 'y))
;; 					 (list (assoc-ref (car a) 'x) (assoc-ref (car a) 'y)))
;; 		(assoc-ref (car a) 'size))
;; 	     (killed-ship? s (cdr a))))))

;; (define (kill-asteroids s a)
;;   (define (f1 s1 a)
;;     (cond ((null? a)
;; 	   (values a #f))
;; 	  (else
;; 	   (let ((a1 (car a)))
;; 	     (cond ((< (distance-between-points (list (assoc-ref s1 'x) (assoc-ref s1 'y))
;; 						(list (assoc-ref a1 'x) (assoc-ref a1 'y)))
;; 		       (assoc-ref a1 'size))
;; 		    (values (cdr a) #t))
;; 		   (else
;; 		    (receive (an k) (f1 s1 (cdr a))
;; 			     (values (cons a1 an) k))))))))

;;   (cond ((null? s)
;; 	 (values s a))
;; 	(else
;; 	 (let ((s1 (car s)))
;; 	   (receive (an k) (f1 s1 a)
;; 		    (cond (k
;; 			   (kill-asteroids (cdr s) an))
;; 			  (else
;; 			   (receive (sn an) (kill-asteroids (cdr s) an)
;; 				    (values (cons s1 sn) an)))))))))
			   

;; (let ((asteroids #f) (ship #f) (shots #f))
;;   (define (new-game n)
;;     (set! asteroids (make-asteroids n))
;;     (set! ship '((x . 0) (y . 0) (angle . 0) (moving . #f)))
;;     (set! shots '()))

;;   (new-game 2)

;;   (run-game
;;    (cond ((killed-ship? ship asteroids)
;; 	  (new-game 2)))
;;    (receive (s a) (kill-asteroids shots asteroids)
;; 	    (set! shots s)
;; 	    (set! asteroids a))
;;    (set! asteroids (map move-asteroid asteroids))
;;    (set! ship (move-ship (alist-copy ship)))
;;    (let ((shot (ship-shot ship)))
;;      (cond (shot
;; 	    (set! shots (cons shot shots)))))
;;    (set! shots (move-shots shots))
;;    (for-each draw-asteroid asteroids)
;;    (for-each draw-shot shots)
;;    (draw-ship ship)))
