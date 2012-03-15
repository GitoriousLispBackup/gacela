;;; Guile configuration

(defun launch-guile ()
  (interactive)
  (let ((process-connection-type nil))
    (start-process "guile" "guile" "/usr/bin/guile")))

(defun send-to-guile ()
  (interactive)
  (cond ((not (get-process "guile"))
   	 (launch-guile)))

  (process-send-string
   "guile"
   (replace-regexp-in-string
    "\t" " "
    (cond ((use-region-p)
	   (buffer-substring-no-properties (region-beginning) (region-end)))
	  (t
	   (buffer-substring-no-properties (point-min-marker) (point-max-marker)))))))


(define-key global-map [(ctrl x) (ctrl g)] 'send-to-guile)

(define-key-after global-map [menu-bar tools guile] (cons "Guile" (make-sparse-keymap "hoot hoot")) 'games)
(define-key global-map [menu-bar tools guile send] '("Send to Guile" . send-to-guile))
(define-key global-map [menu-bar tools guile launch] '("Launch Guile" . launch-guile))

