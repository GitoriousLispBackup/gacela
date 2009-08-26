(in-package :gacela)

(setq *zoom* -50)

(defun tetramine-i ()
  (let ((color '(1 0 0)))
    `((,color ,color ,color ,color))))

(defun tetramine-j ()
  (let ((color '(1 0.5 0)))
    `((,color ,color ,color)
      (nil nil ,color))))

(defun tetramine-l ()
  (let ((color '(1 0 1)))
    `((nil nil ,color)
      (,color ,color ,color))))

(defun tetramine-o ()
  (let ((color '(0 0 1)))
    `((,color ,color)
      (,color ,color))))

(defun tetramine-s ()
  (let ((color '(0 1 0)))
    `((nil ,color ,color)
      (,color ,color nil))))

(defun tetramine-t ()
  (let ((color '(0.5 0 0)))
    `((,color ,color ,color)
      (nil ,color nil))))

(defun tetramine-z ()
  (let ((color '(0 1 1)))
    `((,color ,color nil)
      (nil ,color ,color))))

(defun random-tetramine ()
  (let ((n (random 7)))
    (cond ((= n 0) (tetramine-i))
	  ((= n 1) (tetramine-j))
	  ((= n 2) (tetramine-l))
	  ((= n 3) (tetramine-o))
	  ((= n 4) (tetramine-s))
	  ((= n 5) (tetramine-t))
	  ((= n 6) (tetramine-z)))))

(defun draw-cell (cell)
  (cond ((null cell) nil)
	(t (draw-color cell) (draw-square :size 0.9))))

(defun draw-row (row)
  (mapcar (lambda (cell) (draw-cell cell) (translate 2 0)) row))

(defun draw-grid (grid)
  (mapcar (lambda (row) (draw-row row) (translate (* -2 (length row)) -2)) grid))

(defun join-rows (source destination &optional (offset 0))
  (cond ((null source) destination)
	((null destination) nil)
	((> offset 0) (cons (car destination) (join-rows source (cdr destination) (- offset 1))))
	(t (cons (or (car source) (car destination))
		 (join-rows (cdr source) (cdr destination) offset)))))

(defun join-grids (source destination &optional (x 0) (y 0))
  (cond ((null source) destination)
	((null destination) nil)
	((> y 0) (cons (car destination)
		       (join-grids source (cdr destination) x (- y 1))))
	(t (cons (join-rows (car source) (car destination) x)
		 (join-grids (cdr source) (cdr destination) x y)))))

(defun collide-rows (row1 row2 &optional (offset 0))
  (cond ((not (or row1 row2)) nil)
	((> offset 0) (collide-rows row1 (cdr row2) (- offset 1)))
	(t (or (and (car row1) (car row2)) (collide-rows (cdr row1) (cdr row2))))))

(defun collide-grids (grid1 grid2 &optional (x 0) (y 0))
  (cond ((not (or grid1 grid2)) nil)
	((> y 0) (collide-grids grid1 (cdr grid2) x (- y 1)))
	(t (or (collide-rows (car grid1) (car grid2) x)
	       (collide-grids (cdr grid1) (cdr grid2) x y)))))

(defun rotate-tetramine (grid)
  (labels ((rot (grid res)
		(cond ((null grid) res)
		      (t (rot (cdr grid) (mapcar #'cons (car grid) res))))))
	  (rot grid (make-list (length (car grid))))))

(defun row-completed (row)
  (cond ((null row) t)
	(t (and (car row) (row-completed (cdr row))))))

(defun remove-rows-completed (grid)
  (let ((res (remove-if (lambda (x) (row-completed x)) grid)))
    (labels ((fill (grid n)
		   (cond ((< n 1) grid)
			 (t (fill (cons (make-list 14) grid) (- n 1))))))
	    (fill res (- 20 (length res))))))

(let ((tetramine (random-tetramine)) (x 6) (y 0)
      (next (random-tetramine))
      (timer (make-timer))
      (grid (make-list 20 :initial-element (make-list 14)))
      (background (draw-image-function "fondo_tetris.png")))
  (defun tetramine ()
    (cond ((eq (timer-state timer) 'stopped) (start-timer timer)))

    (cond ((key? 'right)
	   (cond ((not (collide-grids tetramine grid (+ x 1) y))
		  (incf x)))))
    (cond ((key? 'left)
	   (cond ((not (collide-grids tetramine grid (- x 1) y))
		  (decf x)))))
    (cond ((< x 0) (setq x 0))
	  ((> (+ x (length (car tetramine))) 14) (setq x (- 14 (length (car tetramine))))))

    (cond ((key-pressed? 'up)
	   (let ((t1 (rotate-tetramine tetramine)))
	     (cond ((not (collide-grids t1 grid x y))
		    (setq tetramine t1))))))

    (cond ((or (key? 'down) (> (get-time timer) 5000))
	   (cond ((or (collide-grids tetramine grid x (+ y 1))
		      (> (+ y 1 (length tetramine)) 20))
		  (setq grid (remove-rows-completed (join-grids tetramine grid x y)))
		  (setq tetramine next x 6 y 0)
		  (setq next (random-tetramine)))
		 (t (incf y) (start-timer timer)))))

    (funcall background)))
;    (translate -25 19)
;    (draw-grid (join-grids tetramine grid x y))
;    (translate 40 40)
;    (draw-grid next)))

(run-game "Gacela Tetris" (tetramine))
