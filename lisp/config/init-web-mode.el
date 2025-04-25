(straight-use-package 'web-mode)

(dolist (file-ext '("\\.astro\\'"
		    "\\.js\\'"
		    "\\.jsx\\'"
		    "\\.ts\\'"
		    "\\.tsx\\'"))

  (add-to-list 'auto-mode-alist
	       `(,file-ext . web-mode)))

(provide 'init-web-mode)
