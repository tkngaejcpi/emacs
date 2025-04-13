(straight-use-package 'paredit)

(add-hook 'emacs-lisp-mode-hook
	  #'paredit-mode)

(provide 'init-paredit)
