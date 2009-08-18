(show-mob (make-mob :x 0 :y 0 :image (filled-rect 5 420) :tags '(wall)))
(show-mob (make-mob :x 635 :y 0 :image (filled-rect 5 420) :tags '(wall)))
(show-mob (make-mob :x 0 :y 0 :image (filled-rect 640 5) :tags '(wall)))

(show-mob (make-mob :x 280 :y 420 :image (filled-rect 80 20) :tags '(wall)
		    :logic (movement-with-cursors :xvel 200 :yvel 0)))

(let ((xvel 100) (yvel -100))
  (show-mob (make-mob :x 300 :y 200 :image (filled-circle 7)
		      :logic (progn
			       (cond ((> y 480) (setq x 300 y 200 xvel 100 yvel -100))
				     (t (let ((c (collision '(wall))))
					  (cond ((null c) nil)
						((= c (neg (/ pi 2))) (setq yvel (neg (- yvel 10))))
						((= c (/ pi 2)) (setq yvel (neg (+ yvel 10))))
						((= c 0) (setq xvel (neg (+ xvel 10))))
						((= c pi) (setq xvel (neg (- xvel 10))))))))
			       (movement :xvel xvel :yvel yvel)))))

(run-game)
(quit-game)
