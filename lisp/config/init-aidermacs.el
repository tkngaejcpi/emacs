(straight-use-package 'aidermacs)

(setq aidermacs-backend 'vterm)

(keymap-set global-map
	    "C-c a"
	    #'aidermacs-transient-menu)

(provide 'init-aidermacs)
