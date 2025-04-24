(require '+language-server)

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
	       `(python-mode
		 ,(format "%s/basedpyright-langserver"
			  (+language-server-uv-get-excutable-directory 'basedpyright))
		 "--stdio")))

(add-hook 'python-mode-hook
	  #'eglot-ensure)

(provide 'init-eglot)
