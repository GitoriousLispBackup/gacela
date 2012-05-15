#!/usr/bin/guile \
-e gacela-script -s
!#

(use-modules (gacela gacela))
(init-gacela)

(set-game-properties! #:title "Guybrush")

(define-mob (guybrush
	     (guy (load-texture "guybrush.png"))
	     (x 0)
	     (y (+ (- (/ (assoc-ref (get-game-properties) 'height) 2))
		   (/ (assoc-ref (get-texture-properties guy) 'height) 2))))

  (cond ((key? 'left) (set! x (- x 10))))
  (cond ((key? 'right) (set! x (+ x 10))))

  (translate x y)
  (draw-texture guy))

(show-mob (guybrush))

