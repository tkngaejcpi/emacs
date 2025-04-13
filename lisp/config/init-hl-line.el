(dolist (hook '(prog-mode-hook
		dired-mode-hook
		ibuffer-mode-hook))
  (add-hook hook #'hl-line-mode))

(provide 'init-hl-line)
