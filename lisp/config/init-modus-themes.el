(straight-use-package 'modus-themes)

(add-hook 'after-init-hook
	  (lambda ()
	    (load-theme 'modus-operandi-tinted t)))

(provide 'init-modus-themes)
