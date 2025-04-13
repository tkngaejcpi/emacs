(straight-use-package 'avy)

(setq avy-timeout-seconds 0.5)

(keymap-set global-map
	    "C-."
	    #'avy-goto-char-timer)

(provide 'init-avy)
