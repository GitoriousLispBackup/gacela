(set-game-properties! #:title "Gacela Tetris" #:fps 15)

(define (tetramine-i)
  (let ((color '(1 0 0)))
    `((,color ,color ,color ,color))))

(define (tetramine-j)
  (let ((color '(1 0.5 0)))
    `((,color ,color ,color)
      (#f #f ,color))))

(define (tetramine-l)
  (let ((color '(1 0 1)))
    `((#f #f ,color)
      (,color ,color ,color))))

(define (tetramine-o)
  (let ((color '(0 0 1)))
    `((,color ,color)
      (,color ,color))))

(define (tetramine-s)
  (let ((color '(0 1 0)))
    `((#f ,color ,color)
      (,color ,color #f))))

(define (tetramine-t)
  (let ((color '(0.5 0 0)))
    `((,color ,color ,color)
      (#f ,color #f))))

(define (tetramine-z)
  (let ((color '(0 1 1)))
    `((,color ,color #f)
      (#f ,color ,color))))

(define (random-tetramine)
  (let ((n (random 7)))
    (cond ((= n 0) (tetramine-i))
	  ((= n 1) (tetramine-j))
	  ((= n 2) (tetramine-l))
	  ((= n 3) (tetramine-o))
	  ((= n 4) (tetramine-s))
	  ((= n 5) (tetramine-t))
	  ((= n 6) (tetramine-z)))))

(define (draw-cell cell)
  (cond ((and cell (not (null? cell)))
	 (with-color cell (draw-square #:size 20)))))

(define (draw-row row)
  (for-each (lambda (cell) (draw-cell cell) (translate 23 0)) row))

(define (draw-grid grid)
  (for-each (lambda (row) (draw-row row) (translate (* -23 (length row)) -23)) grid))

(define* (join-rows source destination #:optional (offset 0))
  (cond ((null? source) destination)
	((null? destination) '())
	((> offset 0) (cons (car destination) (join-rows source (cdr destination) (- offset 1))))
	(else (cons (or (car source) (car destination))
		    (join-rows (cdr source) (cdr destination) offset)))))

(define* (join-grids source destination #:optional (x 0) (y 0))
  (cond ((null? source) destination)
	((null? destination) '())
	((> y 0) (cons (car destination)
		       (join-grids source (cdr destination) x (- y 1))))
	(else (cons (join-rows (car source) (car destination) x)
		    (join-grids (cdr source) (cdr destination) x y)))))

(define* (collide-rows row1 row2 #:optional (offset 0))
  (cond ((or (null? row1) (null? row2)) #f)
	((> offset 0) (collide-rows row1 (cdr row2) (- offset 1)))
	(else (or (and (car row1) (car row2)) (collide-rows (cdr row1) (cdr row2))))))

(define* (collide-grids grid1 grid2 #:optional (x 0) (y 0))
  (cond ((or (null? grid1) (null? grid2)) #f)
	((> y 0) (collide-grids grid1 (cdr grid2) x (- y 1)))
	(else (or (collide-rows (car grid1) (car grid2) x)
		  (collide-grids (cdr grid1) (cdr grid2) x y)))))

(define (rotate-tetramine grid)
  (define (rot grid res)
    (cond ((null? grid) res)
	  (else (rot (cdr grid) (map cons (car grid) res)))))
  (rot grid (make-list (length (car grid)))))

(define (row-completed row)
  (cond ((null? row) #t)
	(else (and (car row) (row-completed (cdr row))))))

(define (remove-rows-completed grid)
  (let ((res (filter (lambda (x) (not (row-completed x))) grid)))
    (define (fill grid n)
      (cond ((< n 1) grid)
	    (else (fill (cons (make-list 14 #f) grid) (- n 1)))))
    (inc-points (- (length grid) (length res)))
    (fill res (- 20 (length res)))))

(define get-points #f)
(define get-lines #f)
(define inc-points #f)

(let ((points 0) (lines 0))
  (set! get-points
	(lambda ()
	  points))

  (set! get-lines
	(lambda ()
	  lines))

  (set! inc-points
	(lambda (l)
	  (define (more-lines-better n)
	    (cond ((= n 0) n)
		  (else (+ n (more-lines-better (- n 1))))))
	  (set! points (+ points (* (more-lines-better l) 10)))
	  (set! lines (+ lines l)))))

(define game #f)
(define display-game-over #f)
(define tetramine #f)

(let ((current-tetramine (random-tetramine)) (x 6) (y 0)
      (next (random-tetramine))
      (timer (make-timer))
      (grid (make-list 20 (make-list 14 #f)))
      (background (load-texture "fondo_tetris.png"))
      (font (load-font "lazy.ttf" #:size 20))
      (game-over #f))

  (set! game
	(lambda ()
	  (if game-over (display-game-over) (tetramine))))

  (set! display-game-over
	(lambda ()
	  (translate -100 0)
	  (render-text "Game Over" font #:size 50)))

  (set! tetramine
	(lambda ()
	  (cond ((eq? (get-state timer) 'stopped) (start-timer timer)))

	  (cond ((key? 'right)
		 (cond ((not (collide-grids current-tetramine grid (+ x 1) y))
			(set! x (+ x 1))))))
	  (cond ((key? 'left)
		 (cond ((not (collide-grids current-tetramine grid (- x 1) y))
			(set! x (- x 1))))))
	  (cond ((< x 0) (set! x 0))
		((> (+ x (length (car current-tetramine))) 14) (set! x (- 14 (length (car current-tetramine))))))

	  (cond ((key-pressed? 'up)
		 (let ((t1 (rotate-tetramine current-tetramine)))
		   (cond ((not (collide-grids t1 grid x y))
			  (set! current-tetramine t1))))))

	  (cond ((or (key? 'down) (> (get-time timer) 5000))
		 (cond ((or (collide-grids current-tetramine grid x (+ y 1))
			    (> (+ y 1 (length current-tetramine)) 20))
			(set! grid (remove-rows-completed (join-grids current-tetramine grid x y)))
			(set! current-tetramine next)
			(set! x 6)
			(set! y 0)
			(cond ((collide-grids current-tetramine grid x y) (set! game-over #t)))
			(set! next (random-tetramine)))
		       (else
			(set! y (+ y 1))
			(start-timer timer)))))
	  (draw-texture background)
	  (translate -288 218)
	  (draw-grid (join-grids current-tetramine grid x y))
	  (translate 440 440)
	  (draw-grid next)
	  (translate -40 -100)
	  (render-text (format #f "Points: ~a" (get-points)) font)
	  (translate 0 -30)
	  (render-text (format #f "Lines: ~a" (get-lines)) font))))

(define (run-gacela-tetris)
  (let ((frame 0.0) (fps (make-timer)) (update (make-timer)))
    (start-timer update)
    (start-timer fps)
    (run-game
     (game)
     (set! frame (+ frame 1))
     (cond ((> (get-time update) 1000)
	    (display (/ frame (/ (get-time fps) 1000.0)))
	    (newline)
	    (start-timer update))))))
