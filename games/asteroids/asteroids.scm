(set-game-properties! #:title "Gacela Asteroids")

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
    (cond ((> nx 320) (set! vx -1))
	  ((< nx -320) (set! vx 1)))
    (cond ((> ny 240) (set! vy -1))
	  ((< ny -240) (set! vy 1)))
    (list (+ x vx) (+ y vy) vx vy)))

(let ((asteroids '((100 100 1 1) (-100 -100 -1 1))))
  (run-game
   (set! asteroids (map move-asteroid asteroids))
   (for-each draw-asteroid asteroids)))
