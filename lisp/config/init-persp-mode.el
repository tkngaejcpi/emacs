(straight-use-package 'persp-mode)


(setq persp-auto-save-opt 2)

;; limit buffer commands in corresponding perspective, if persp-mode is on
(keymap-set global-map
	    "C-x b"
	    (lambda ()
	      (interactive)
	      (call-interactively (if persp-mode #'persp-switch-to-buffer #'switch-to-buffer))))

(keymap-set global-map
	    "C-x k"
	    (lambda ()
	      (interactive)
	      (call-interactively (if persp-mode #'persp-kill-buffer #'kill-buffer))))

;; setup persp-switch-perspective-repeat-map to easily switch perspective
(defvar persp-switch-perspective-repeat-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "p" #'persp-prev)
    (keymap-set map "n" #'persp-next)

    (dolist (command '(persp-prev persp-next))
      (put command 'repeat-map 'persp-switch-perspective-repeat-map))
    
    map))

(provide 'init-persp-mode)
