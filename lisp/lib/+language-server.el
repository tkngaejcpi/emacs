(defvar +language-server-directory
  (expand-file-name "language-server"
		    user-emacs-directory))

(defun +language-server-get-server-directory (server)
  (expand-file-name (symbol-name server)
		    +language-server-directory))

(defvar +language-server-uv-command
  "~/.local/bin/uv")

(defun +language-server-uv-install-server (server)
  (shell-command (format "%s --directory %s sync"
			 +language-server-uv-command
			 (+language-server-get-server-directory server))))

(defun +language-server-uv-get-excutable-directory (server)
  (expand-file-name ".venv/bin"
		    (+language-server-get-server-directory server)))

(provide '+language-server)
