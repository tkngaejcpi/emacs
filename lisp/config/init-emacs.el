(setq-default mode-line-format
	      (list
	       "%e"
	       
	       mode-line-mule-info
	       mode-line-modified
	       mode-line-remote
	       
	       mode-line-buffer-identification
	       
	       "  "
	       '(-3 "%p") ;; percentage
	       
	       "  "
	       mode-line-misc-info
	       '(:eval mode-name)
	       
	       "  "
	       '(:eval '(vc-mode vc-mode))

	       'mode-line-format-right-align
	       `(:eval (when persp-mode
			 (format "[%s]" (propertize (persp-name (get-current-persp))
							   'face 'italic))))
	       "  "
	       ))

(add-hook 'window-setup-hook
	  (lambda ()
	    (set-face-attribute 'default
				nil
				:font (font-spec :family "Cascadia Code"
						 :size 14))
	    
	    (set-fontset-font t
			      'unicode
			      "Noto Sans CJK HK")

	    (set-fontset-font t
			      'emoji
			      "Noto Color Emoji")))

(provide 'init-emacs)
