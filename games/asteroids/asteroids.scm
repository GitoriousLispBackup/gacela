(set-game-properties! #:title "Gacela Asteroids")

(define max-x (/ (assoc-ref (get-game-properties) 'width) 2))
(define min-x (- max-x))
(define max-y (/ (assoc-ref (get-game-properties) 'height) 2))
(define min-y (- max-y))

(define-mob (asteroid
	     (image (load-texture "Asteroid.png"))
	     (x 0) (y 0) (angle 0) (dir 0))
  (let ((r (degrees-to-radians (- dir))))
    (set! x (+ x (sin r)))
    (set! y (+ y (cos r))))
  (set! angle (+ angle 1))

  (cond ((or (> x max-x) (< x min-x))
	 (set! dir (* -1 dir))))
  (cond ((or (> y max-y) (< y min-y))
	 (set! dir (- 180 dir))))
  
  (translate x y)
  (rotate angle)
  (draw-texture image))

(define-mob (ship
	     (ship1 (load-texture "Ship1.png"))
	     (ship2 (load-texture "Ship2.png"))
	     (x 0) (y 0) (angle 0)
	     (moving #f))
  (cond ((key? 'left) (set! angle (+ angle 5)))
	((key? 'right) (set! angle (- angle 5))))
  (cond ((key? 'up)
	 (let ((r (degrees-to-radians (- angle))))
	   (set! x (+ x (* 4 (sin r))))
	   (set! y (+ y (* 4 (cos r)))))
	 (cond ((> x max-x) (set! x min-x))
	       ((< x min-x) (set! x max-x)))
	 (cond ((> y max-y) (set! y min-y))
	       ((< y min-y) (set! y max-y)))
	 (set! moving #t))
	(else
	 (set! moving #f)))
  (cond ((key-pressed? 'space)
	 (show-mob (make-shot #:x x #:y y #:angle angle))))

  (translate x y)
  (rotate angle)
  (draw-texture (if moving ship2 ship1)))

(define-mob (shot (x 0) (y 0) (angle 0))
  (let ((r (degrees-to-radians (- angle))))
    (set! x (+ x (* 10 (sin r))))
    (set! y (+ y (* 10 (cos r))))
    (cond ((or (> x max-x)
	       (< x min-x)
	       (> y max-y)
	       (< y min-y))
	   (kill-me))))

  (translate x y)
  (rotate angle)
  (draw-line 10))


(define (init-asteroids n)
  (cond ((> n 0)
	 (let ((x (- (random (* max-x 2)) max-x))
	       (y (- (random (* max-y 2)) max-y)))
	   (cond ((< (distance-between-points (list x y) '(0 0)) 120)
		  (init-asteroids n))
		 (else
		  (let ((angle (random 360)) (dir (- (random 360) 180)))
		    (show-mob (make-asteroid #:x x #:y y #:angle angle #:dir dir)))
		  (init-asteroids (- n 1))))))))


(init-asteroids 2)
(show-mob (make-ship))

(let ((font (load-font "../tetris/lazy.ttf" #:size 20)))
  (run-game
   (render-text (format #f "Mobs: ~a" (length (get-active-mobs))) font)))


;;   (define (new-game n)
;;     (set! asteroids (make-asteroids n))
;;     (set! ship '((x . 0) (y . 0) (angle . 0) (moving . #f)))
;;     (set! shots '()))

;;   (new-game 2)

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