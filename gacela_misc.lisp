;;; Gacela, a GNU Common Lisp extension for fast games development
;;; Copyright (C) 2009 by Javier Sancho Fernandez <jsf at jsancho dot org>
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(defconstant INFINITY MOST-POSITIVE-LONG-FLOAT)

(defun append-if (new test tree &key (key #'first) (test-if #'equal))
  (cond ((atom tree) tree)
	(t (append-if-1
	    new
	    test
	    (mapcar (lambda (x) (append-if new test x :key key :test-if test-if)) tree)
	    :key key
	    :test-if test-if))))

(defun append-if-1 (new test tree &key (key #'first) (test-if #'equal))
  (cond ((funcall test-if (funcall key tree) test) (append tree new))
	(t tree)))

(defun car+ (var)
  (if (listp var) (car var) var))

(defun avg (&rest numbers)
  (let ((total 0))
    (dolist (n numbers) (incf total n))
    (/ total (length numbers))))

(defun neg (num)
  (* -1 num))

(defun signum+ (num)
  (let ((sig (signum num)))
    (cond ((= sig 0) 1)
	  (t sig))))

(defmacro destructure (destructuring-list &body body)
  (let ((lambda-list nil) (exp-list nil))
    (dolist (pair destructuring-list)
      (setq exp-list (cons (car pair) exp-list))
      (setq lambda-list (cons (cadr pair) lambda-list)))
    `(destructuring-bind ,lambda-list ,(cons 'list exp-list) ,@body)))

(defun match-pattern (list pattern)
  (cond ((and (null list) (null pattern)) t)
	((and (consp list) (consp pattern))
	 (and (match-pattern (car list) (car pattern)) (match-pattern (cdr list) (cdr pattern))))
	((and (atom list) (atom pattern))
	 (cond ((or (numberp list) (numberp pattern)) (and (numberp list) (numberp pattern)))
	       (t t)))))

(defun nearest-power-of-two (n)
  (labels ((power (p n)
		  (cond ((> (* p 2) n) p)
			(t (power (* p 2) n)))))
	  (power 1 n)))

(defmacro secure-block (output-stream &rest forms)
  (let ((error-handler #'si::universal-error-handler))
    `(block secure
       (defun si::universal-error-handler (error-name correctable function-name continue-format-string error-format-string &rest args)
	 ,(when output-stream
	    `(write-line
	      (cond ((eq error-name :WRONG-TYPE-ARGUMENT) (string error-name))
		    (t error-format-string))
	      ,output-stream))
	 (setf (symbol-function 'si::universal-error-handler) ,error-handler)
	 (return-from secure))
       (let (result-eval)
	 (setq result-eval (progn ,@forms))
	 (setf (symbol-function 'si::universal-error-handler) ,error-handler)
	 result-eval))))

(defmacro persistent-let (name vars &rest forms)
  (labels ((get-vars (vars)
		     (cond ((null vars) nil)
			   (t (cons (if (consp (car vars)) (caar vars) (car vars))
				    (get-vars (cdr vars)))))))
   
	  `(let ,(cond ((functionp name)
			(let ((old-vars (funcall name)))
			  (cond ((equal (get-vars vars) (get-vars old-vars)) old-vars)
				(t vars))))
		       (t vars))
	     (defun ,name ()
	       ,(let ((lvars (get-vars vars)))
		  `(mapcar (lambda (x y) (list x y)) ',lvars ,(cons 'list lvars))))
	     ,@forms)))

;Geometry
(defun dotp (dot)
  (match-pattern dot '(0 0)))

(defun vectorp (vector)
  (match-pattern vector '(0 0)))

(defun circlep (circle)
  (match-pattern circle '((0 0) 0)))

(defun polygonp (polygon)
  (cond ((consp polygon)
	 (and (dotp (car polygon))
	      (if (null (cdr polygon)) t (polygonp (cdr polygon)))))))

(defun make-dot (x y)
  `(,x ,y))

(defun make-vector (x y)
  `(,x ,y))

(defun make-line (dot1 dot2)
  `(,dot1 ,dot2))

(defun make-rectangle (x1 y1 x2 y2)
  `((,x1 ,y1) (,x2 ,y1) (,x2 ,y2) (,x1 ,y2)))

(defun polygon-center (polygon)
  (apply #'mapcar #'avg polygon))

(defun dots-distance (dot1 dot2)
  (destructure ((dot1 (x1 y1))
		(dot2 (x2 y2)))
	       (sqrt (+ (expt (- x2 x1) 2)
			(expt (- y2 y1) 2)))))

(defun dot-line-distance (dot line)
  (destructure ((line ((ax ay) (bx by)))
		(dot (cx cy)))
	       (let* ((r-numerator (+ (* (- cx ax) (- bx ax)) (* (- cy ay) (- by ay))))
		      (r-denomenator (+ (expt (- bx ax) 2) (expt (- by ay) 2)))
		      (r (/ r-numerator r-denomenator)))
		 (values
		  (* (abs (/ (- (* (- ay cy) (- bx ax)) (* (- ax cx) (- by ay)))
			     r-denomenator))
		     (sqrt r-denomenator))
		  r))))

(defun dot-segment-distance (dot segment)
  (multiple-value-bind
   (dist r) (dot-line-distance dot segment)
	(cond ((and (>= r 0) (<= r 1)) dist)
	      (t (let ((dist1 (dots-distance dot (car segment)))
		       (dist2 (dots-distance dot (cadr segment))))
		   (if (< dist1 dist2) dist1 dist2))))))

(defun perpendicular-line (dot line)
  (destructure ((line ((ax ay) (bx by))))
	       (multiple-value-bind
		(dist r) (dot-line-distance dot line)
		(make-line dot
			   (make-dot (+ ax (* r (- bx ax)))
				     (+ ay (* r (- by ay))))))))

(defun line-angle (line)
  (destructure ((line ((ax ay) (bx by))))
	       (let ((x (- bx ax)) (y (- by ay)))
		 (if (and (= x 0) (= y 0)) 0 (atan y x)))))

(defun inverse-angle (angle)
  (cond ((< angle pi) (+ angle pi))
	(t (- angle pi))))

(defun translate-dot (dot dx dy)
  (destructure ((dot (x y)))
	       (list (+ x dx) (+ y dy))))

(defun translate-circle (circle dx dy)
  (destructure ((circle (center radius)))
	       (list (translate-dot center dx dy) radius)))

(defun translate-polygon (pol dx dy)
  (mapcar (lambda (dot)
	    (translate-dot dot dx dy))
	  pol))

(defun polygon-edges (pol)
  (mapcar (lambda (v1 v2) (list v1 v2))
	  pol
	  (union (cdr pol) (list (car pol)))))

(defun polygon-dot-intersection (polygon dot)
;Eric Haines algorithm
  (let ((edges (polygon-edges
		(translate-polygon polygon (neg (car dot)) (neg (cadr dot)))))
	(counter 0))
    (dolist (edge edges)
      (destructure ((edge ((x1 y1) (x2 y2))))
		   (cond ((/= (signum+ y1) (signum+ y2))
			  (cond ((and (> x1 0) (> x2 0)) (incf counter))
				((and (or (> x1 0) (> x2 0))
				      (> (- x1 (* y1 (/ (- x2 x1) (- y2 y1)))) 0))
				 (incf counter)))))))
    (not (evenp counter))))

(defun circle-segment-intersection (circle segment)
  (destructure ((circle (center radius)))
	       (<= (dot-segment-distance center segment) radius)))

(defun circle-edges-intersection (circle polygon)
  (let ((edges (polygon-edges polygon))
	(edges-i nil))
    (dolist (edge edges)
      (cond ((circle-segment-intersection circle edge) (setq edges-i (cons edge edges-i)))))
    edges-i))

(defun circle-polygon-intersection (circle polygon)
  (or (polygon-dot-intersection polygon (car circle))
      (circle-edges-intersection circle polygon)))

(defun circle-circle-intersection (circle1 circle2)
  (destructure ((circle1 (center1 radius1))
		(circle2 (center2 radius2)))
	       (<= (dots-distance center1 center2) (+ r1 r2))))
