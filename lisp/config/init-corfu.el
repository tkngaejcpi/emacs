(straight-use-package 'corfu)

(setq corfu-auto t)

(add-hook 'after-init-hook
	  #'global-corfu-mode)

(provide 'init-corfu)
