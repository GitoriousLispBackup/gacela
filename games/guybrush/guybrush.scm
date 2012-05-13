#!/usr/bin/guile \
-e gacela-script -s
!#

(use-modules (gacela gacela))
(init-gacela)

(set-game-properties! #:title "Guybrush")

(define-mob (guybrush
	     (guy (load-texture "guybrush.png"))
	     (x 0)
	     (y (- (/ (assoc-ref (get-game-properties) 'height) 2))))
  (translate x y)
  (draw-texture guy))

(show-mob (guybrush))

;; Tengo que hacer una función para obtener propiedades de textura en video.scm