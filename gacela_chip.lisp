(in-package 'chipmunk)

(clines "#include \"gacela_chipmunk.c\"")

(defconstant INFINITY MOST-POSITIVE-LONG-FLOAT)

;;; Chipmunk functions
(defentry cpInitChipmunk () (void "gacela_cpInitChipmunk"))
(defentry cpResetShapeIdCounter () (void "gacela_cpResetShapeIdCounter"))
(defentry cpSpaceNew () (int "gacela_cpSpaceNew"))
(defentry cpSpaceAddBody (int int) (void "gacela_cpSpaceAddBody"))
(defentry cpSpaceAddShape (int int) (void "gacela_cpSpaceAddShape"))
(defentry cpSpaceFree (int) (void "gacela_cpSpaceFree"))
(defentry cpBodyNew (float float float) (int "gacela_cpBodyNew"))
(defentry cpBodyFree (int) (void "gacela_cpBodyFree"))
(defentry cpCircleShapeNew (int float float float) (int "gacela_cpCircleShapeNew"))
(defentry cpPolyShapeNew (int int int float float) (int "gacela_cpPolyShapeNew"))
(defentry cpShapeFree (int) (void "gacela_cpShapeFree"))

;;; C-Gacela functions
(defentry set-space-properties (int float float) (void "set_space_properties"))

;;; Physics Subsystem
(defstruct space address)
(defstruct body address)
(defstruct shape address)

(let ((initialized nil)
      (mobs-space nil))

  (defun init-chipmunk ()
    (cond ((null initialized) (cpInitChipmunk) (setq initialized t))
	  (t initialized)))

  (defun init-mobs-physics (&key (gravity nil))
    (cond ((null mobs-space) (init-chipmunk) (setq mobs-space (create-space)))
	  (t mobs-space))))

(defun create-space (&key (gravity nil))
  (init-chipmunk)
  (let ((new-space (make-space :address (cpSpaceNew)))
	(properties nil))
    (set-resource 'space new-space (gentemp))
    (cond (gravity (setq properties (union gravity properties))))
    (cond (properties (apply #'set-space-properties (cons (space-address new-space) properties))))
    new-space))

(defun create-body (&key (mass INFINITY) (inertia INFINITY))
  (init-chipmunk)
  (let ((new-body (make-body :address (cpNewBody mass inertia INFINITY))))
    (set-resource 'body new-body (gentemp))
    new-body))
