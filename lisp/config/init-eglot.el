(defvar +eglot-uvx-command
  (expand-file-name "~/.local/bin/uvx"))

(defvar +eglot-bunx-command
  (expand-file-name "~/.bun/bin/bunx"))

;; tell eglot how to start language servers
(with-eval-after-load 'eglot
  (dolist (config
	   `((python-mode
	      ,+eglot-uvx-command
	      "--from"
	      "basedpyright"
	      "basedpyright-langserver"
	      "--stdio")

	     (web-mode
	      ,+eglot-bunx-command
	      "typescript-language-server"
	      "--stdio")
	     ))

    (add-to-list 'eglot-server-programs config)))

(dolist (hook '(python-mode-hook
		web-mode-hook))
  (add-hook hook #'eglot-ensure))

(provide 'init-eglot)
