(straight-use-package 'web-mode)

(add-to-list 'auto-mode-alist
	     '("\\.astro\\'" . web-mode))

(use-package web-mode
  :mode ("\\.astro\\'" . web-mode))

(provide 'init-web-mode)
