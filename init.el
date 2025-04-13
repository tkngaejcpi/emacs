(add-to-list 'load-path
	     (expand-file-name "lisp/lib"
			       user-emacs-directory))

(add-to-list 'load-path
	     (expand-file-name "lisp/config"
			       user-emacs-directory))

(require '+init)
