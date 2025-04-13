(setq inhibit-startup-screen t)
(setq make-backup-files nil)

(setq-default cursor-type 'bar)

(setq default-frame-alist '((width . 90)
                            (height . 50)
                            (alpha-background . 95)))

(setq-default mode-line-format
	      (list
	       "%e"
	       mode-line-front-space
	       
	       mode-line-mule-info
	       mode-line-client
	       mode-line-modified
	       mode-line-remote
	       
	       mode-line-buffer-identification
	       
	       "  "
	       '(-3 "%p") ;; percentage
	       
	       "  "
	       mode-line-misc-info
	       '(:eval mode-name)
	       
	       "  "
	       '(:eval `(vc-mode vc-mode))
	       
	       mode-line-end-spaces))

(add-hook 'after-init-hook
	  (lambda ()
	    (menu-bar-mode -1)
	    (tool-bar-mode -1)
	    (scroll-bar-mode -1)))

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
