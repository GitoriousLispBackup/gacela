(defmacro defproc (name type variables init logic motion)
  `(let ((make-name ',(intern (concatenate 'string "MAKE-" (string name)))))
     (setf (symbol-function make-name)
	   (make-proc-constructor ,type ,variables ,init ,logic ,motion))
     make-name))

(defmacro make-proc-constructor (type variables init logic motion)
  `(lambda
     ,(if (null variables) () (cons '&key variables))
     (proc-structure ,type ,variables ,init ,logic ,motion)))

(defmacro proc-structure (type variables init logic motion)
  `(list
    :type ,type
    :init (lambda () ,init)
    :logic (lambda () ,logic)
    :motion (lambda () ,motion)
    :context (lambda ()
	       ,(if variables
		    `(mapcar #'list
			     ',(mapcar #'car+ variables)
			     (multiple-value-list
			      (values-list ,(cons 'list (mapcar #'car+ variables)))))
		  nil))))

(defun proc-value (proc label)
  (car (cdr (assoc label (funcall (getf proc :context))))))

(defun proc-type (proc)
  (getf proc :type))

(defun init-proc (proc)
  (funcall (getf proc :init)))

(defun logic-proc (proc)
  (funcall (getf proc :logic)))

(defun motion-proc (proc)
  (funcall (getf proc :motion)))

(let ((active-procs nil) (procs-to-add nil) (procs-to-quit nil))

  (defun add-proc (proc)
    (push proc procs-to-add))

  (defun logic-procs ()
    (dolist (proc active-procs) (logic-proc proc)))

  (defun motion-procs ()
    (dolist (proc active-procs) (motion-proc proc)))

  (defun funcall-procs (func)
    (dolist (proc active-procs) (funcall func proc)))

  (defun filter-procs (test)
    (intersection (mapcar (lambda (p) (cond ((funcall test p) p))) active-procs) active-procs))

  (defun quit-proc (proc)
    (push proc procs-to-quit))

  (defun refresh-active-procs ()
    (do ((proc (pop procs-to-add) (pop procs-to-add))) ((null proc))
	(push proc active-procs)
	(init-proc proc))
    (do ((proc (pop procs-to-quit) (pop procs-to-quit))) ((null proc))
	(setq active-procs (reverse (set-difference active-procs (list proc) :test #'equal)))))

  (defun quit-all-procs ()
    (setq active-procs nil)
    (setq procs-to-add nil)
    (setq procs-to-quit nil)))
