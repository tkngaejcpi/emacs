(setq treesit-language-source-alist '((elisp "https://github.com/Wilfred/tree-sitter-elisp")))

(with-eval-after-load 'treesit
  (dolist (lang (mapcar #'car
			treesit-language-source-alist))
    (unless (treesit-language-available-p lang)
      (treesit-install-language-grammar lang))))

(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (treesit-parser-create 'elisp)))

(provide 'init-treesit)
