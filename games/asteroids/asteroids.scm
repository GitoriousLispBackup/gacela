(set-game-properties! #:title "Gacela Asteroids")

(define max-x (/ (cdr (assoc 'width (get-game-properties))) 2))
(define min-x (- max-x))
(define max-y (/ (cdr (assoc 'height (get-game-properties))) 2))
(define min-y (- max-y))

(define draw-asteroid
  (let ((asteroid (load-texture "Asteroid.png")))
    (lambda (a)
      (to-origin)
      (translate (car a) (cadr a))
      (draw-texture asteroid))))

(define (move-asteroid a)
  (let* ((x (car a)) (y (cadr a))
	 (vx (caddr a)) (vy (cadddr a))
	 (nx (+ x vx)) (ny (+ y vy)))
    (cond ((> nx max-x) (set! vx -1))
	  ((< nx min-x) (set! vx 1)))
    (cond ((> ny max-y) (set! vy -1))
	  ((< ny min-y) (set! vy 1)))
    (list (+ x vx) (+ y vy) vx vy)))

(define draw-ship
  (let ((ship1 (load-texture "Ship1.png"))
	(ship2 (load-texture "Ship2.png")))
    (lambda (s)
      (to-origin)
      (translate (car s) (cadr s))
      (rotate (caddr s))
      (draw-texture ship1))))

(define (move-ship s)
  (let ((x (car s)) (y (cadr s))
	(angle (caddr s))
	(vx (cadddr s)) (vy (cadddr (cdr s))))
    (cond ((key? 'left) (set! angle (+ angle 5)))
	  ((key? 'right) (set! angle (- angle 5))))
    (cond ((key? 'up) (set! y (+ y 3))))
    (list x y angle vx vy)))

(let ((asteroids '((100 100 1 1) (-100 -100 -1 1)))
      (ship '(0 0 0 0 0)))
  (run-game
   (set! asteroids (map move-asteroid asteroids))
   (set! ship (move-ship ship))
   (for-each draw-asteroid asteroids)
   (draw-ship ship)))
