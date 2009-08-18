;;;
;;; Chipmunk Physics Engine
;;;

(clines "#include \"gacela_chipmunk.c\"")

;;; Chipmunk functions
(defentry cpInitChipmunk () (void "gacela_cpInitChipmunk"))
(defentry cpResetShapeIdCounter () (void "gacela_cpResetShapeIdCounter"))
(defentry cpSpaceNew () (int "gacela_cpSpaceNew"))
(defentry cpSpaceAddBody (int int) (void "gacela_cpSpaceAddBody"))
(defentry cpSpaceAddShape (int int) (void "gacela_cpSpaceAddShape"))
(defentry cpSpaceFree (int) (void "gacela_cpSpaceFree"))
(defentry cpBodyNew (float float float) (int "gacela_cpBodyNew"))
(defentry cpMomentForCircle (float float float float float) (float "gacela_cpMomentForCircle"))
(defentry cpBodyFree (int) (void "gacela_cpBodyFree"))
(defentry cpCircleShapeNew (int float float float) (int "gacela_cpCircleShapeNew"))
(defentry cpPolyShapeNew (int int int float float) (int "gacela_cpPolyShapeNew"))
(defentry cpShapeFree (int) (void "gacela_cpShapeFree"))

;;; C-Gacela functions
(defentry set-cp-space-gravity (int float float) (void "set_cp_space_gravity"))

;;; Physics Subsystem
(defstruct cp-space address gravity)
(defstruct cp-body address position)
(defstruct cp-shape address)

(let ((initialized nil)
      (mobs-cp-space nil))

  (defun init-chipmunk ()
    (cond ((null initialized) (cpInitChipmunk) (setq initialized t))
	  (t initialized)))

  (defun init-cp-space (&key (gravity nil))
    (cond ((null mobs-cp-space) (init-chipmunk) (setq mobs-cp-space (create-cp-space)))
	  (t mobs-cp-space)))

  (defun add-cp-body (body)
    (cpSpaceAddBody (cp-space-address mobs-cp-space) (cp-body-address body)))

  (defun add-cp-shape (shape)
    (cpSpaceAddShape (cp-space-address mobs-cp-space) (cp-shape-address shape))))

(defun create-cp-space (&key (gravity nil))
  (init-chipmunk)
  (let ((new-cp-space (make-cp-space :address (cpSpaceNew) :gravity gravity))
	(properties nil))
    (set-resource 'cp-space new-cp-space (gentemp))
    (cond (gravity (setq properties (union gravity properties))))
    (cond (properties (apply #'set-cp-space-properties (cons (cp-space-address new-cp-space) properties))))
    new-cp-space))

(defun create-cp-body (&key (mass INFINITY) (inertia INFINITY) (x 0) (y 0))
  (init-chipmunk)
  (let ((new-cp-body (make-cp-body :address (cpNewBody mass inertia INFINITY) :position `(,x ,y))))
    (set-resource 'cp-body new-cp-body (gentemp))
    new-cp-body))

(defun create-circle-cp-shape (cp-body shape)
  (init-chipmunk)
  (destructure ((shape ((x y) r)))
	       (make-cp-shape :address (cpCircleShapeNew cp-body r x y))))

(defun create-cp-shape (cp-body shape)
  (init-chipmunk)
  (let ((new-cp-shape (cond ((circle-p shape) (create-circle-cp-shape cp-body shape)))))
    (set-resource 'cp-shape new-cp-shape (gentemp))
    new-cp-shape))

(defun cp-moment (mass shape)
  (cond ((circle-p shape) (destructure ((shape ((x y) r))) (cpMomentForCircle mass 0.0 r x y)))
	t INFINITY))

;(defun use-chipmunk ()
;  (defun physics-add-mob (mass shape x y)
;    (init-cp-space)
;    (let ((new-cp-body (create-cp-body mass (cp-moment mass shape))))
;      (add-cp-body new-cp-body)
      