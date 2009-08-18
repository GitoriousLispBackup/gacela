(show-mob (make-mob :x 0 :y 0 :image (filled-rect 5 420) :tags '(wall)
		    :logic (cond ((key 'up) (incf x 5))
				 ((key 'down) (decf x 5)))))

(show-mob (make-mob :x 635 :y 0 :image (filled-rect 5 420) :tags '(wall)
		    :logic (cond ((key 'up) (decf x 5))
				 ((key 'down) (incf x 5)))))

(let ((xvel 100) (yvel 0))
  (show-mob (make-mob :x 300 :y 200 :image (filled-circle 7)
		      :logic (progn
			       (cond ((key 'plus) (if (> xvel 0) (incf xvel 10) (decf xvel 10)))
				     ((key 'minus) (if (> xvel 0) (decf xvel 10) (incf xvel 10))))
			       (cond ((collision '(wall)) (setq xvel (neg xvel))))
			       (movement :xvel xvel :yvel yvel)))))

(run-game)
(quit-game)
