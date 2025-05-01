(straight-use-package 'minuet)
(straight-use-package 'dash)

;;; Provider
(setq minuet-provider "codestral")

(with-eval-after-load 'minuet
  (plist-put minuet-codestral-options
	     :model "codestral-2501")

  (plist-put minuet-codestral-options
	     :api-key (--> "codestral.mistral.ai"
			   (auth-source-search :host it :user "apikey")
			   (car it)
			   (plist-get it :secret)))

  (plist-put minuet-codestral-options
	     :max_tokens 64))

;;; Completion
(setq minuet-n-completions 1)

(add-hook 'prog-mode-hook
	  #'minuet-auto-suggestion-mode)

;;; Key Bindings
(with-eval-after-load 'minuet
  (keymap-set minuet-active-mode-map
	      "M-RET"
	      #'minuet-accept-suggestion))

(provide 'init-minuet)
