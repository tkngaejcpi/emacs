;;; Package
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-vc-git-default-clone-depth 1)

;;; Emacs Lisp
(require 'seq)
(require 'subr-x)

(straight-use-package 'llama)
(require 'llama)

;;; Utils
(defun version>= (version-a version-b)
  (not (version< version-a version-b)))

;;; Assertions
(unless (version>= emacs-version "30.1")
  (error "Emacs version should >= 30.1."))

(unless (treesit-available-p)
  (error "Emacs shoule be compiled with tree sitter."))

(unless (executable-find "uvx")
  (warn "Executable uvx is required for eglot."))

(unless (executable-find "bunx")
  (warn "Executable bunx is required for eglot."))

(unless (executable-find "emacs-lsp-booster")
  (warn "Executable emacs-lsp-booster is required for eglot-booster."))

;;; Appearance

;;;; Frame
(setq default-frame-alist
      '((alpha-background . 95)))

;;;; Bar
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)

;;;; Theme
(straight-use-package 'ef-themes)

(load-theme 'ef-summer t)

;;;; Font
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

;;;; Mode Line
(defun mode-line-persp-indicator ()
  (let* ((persp (get-current-persp))
	 (persp-name (if persp (persp-name persp) "None")))
    
    (propertize (format " %s " persp-name)
		'face (list
		       :inherit 'mode-line
		       :background (ef-themes-with-colors bg-graph-magenta-0)
		       :weight 'bold))))

(setq-default mode-line-format
	      '((:eval (mode-line-persp-indicator))
		
		" "
		mode-line-buffer-identification
		" "

		mode-line-format-right-align
		
		mode-line-percent-position
		"  "))

;;; Completion

;;;; Minibuffer
(straight-use-package 'vertico)
(vertico-mode)

;;;; Buffer
(straight-use-package 'corfu)
(global-corfu-mode)

(setq corfu-auto t)

;;;; Completion Style
(straight-use-package 'orderless)

(setq completion-styles '(orderless basic))
(setq completion-category-overrides '((file (styles basic partial-completion))))

;;; Editing

;;;; Highlight Line
(global-hl-line-mode)

(dolist (hook '(vterm-mode-hook))
  (add-hook hook
	    (lambda ()
	      (hl-line-mode 'toggle))))

;;;; Line Number
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;;;; Repeat
(repeat-mode)

;;;; Emacs Lisp
(straight-use-package 'paredit)
(add-hook 'emacs-lisp-mode-hook #'paredit-mode)

;;; Extra Modes
(straight-use-package 'markdown-mode)
(straight-use-package 'astro-ts-mode)

;;; Workspace

;;;; Basic
(straight-use-package 'persp-mode)

(setq persp-auto-save-opt 2)
(setq persp-save-dir (file-truename "~/.cache/emacs/persp-confs/"))

(persp-mode 1)

;;;; Hooks
(add-hook 'dired-mode-hook (## persp-add-buffer (current-buffer)))

;; Key Binding
(keymap-set persp-mode-map "C-x b" #'persp-switch-to-buffer)
(keymap-set persp-mode-map "C-x k" #'persp-kill-buffer)

(keymap-set persp-mode-map "C-c p <left>" #'persp-prev)
(keymap-set persp-mode-map "C-c p <right>" #'persp-next)

(defvar persp-switch-repeat-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "<left>" #'persp-prev)
    (keymap-set map "<right>" #'persp-next)
    map))

(dolist (command '(persp-prev persp-next))
  (put command 'repeat-map 'persp-switch-repeat-map))

;;; Treesit

;;;; Source
(setq treesit-language-source-alist
      '((elisp . ("https://github.com/Wilfred/tree-sitter-elisp" "1.5.0" "src"))
	(css .  ("https://github.com/tree-sitter/tree-sitter-css" "v0.23.2" "src"))
	(typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.23.2" "typescript/src"))
	(tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.23.2" "tsx/src"))
	(astro . ("https://github.com/virchau13/tree-sitter-astro" "master" "src"))))

;;;; Grammar
(with-eval-after-load 'treesit
  (thread-last
    treesit-language-source-alist
    (seq-map #'car)
    (seq-filter (## thread-last %1
		    (treesit-language-available-p)
		    (not)))
    (seq-do #'treesit-install-language-grammar)))

;;; Language Server

;;;; Servers
(with-eval-after-load 'eglot
  (let ((bunx (executable-find "bunx"))
	(uvx (executable-find "uvx")))

    (seq-do (## add-to-list 'eglot-server-programs %1)
	    `(((python-mode python-ts-mode)
		 ,uvx
		 "--from"
		 "basedpyright"
		 "basedpyright-langserver"
		 "--stdio")

		(typescript-ts-mode
		 ,bunx
		 "typescript-language-server"
		 "--stdio")

		(astro-ts-mode
		 ,bunx
		 "@astrojs/language-server"
		 "--stdio"
		 :initializationOptions (:typescript (:tsdk "./node_modules/typescript/lib")))))))

(thread-last '(python-mode
	       python-ts-mode
	       typescript-ts-mode
	       astro-ts-mode)

	     (seq-map (##
		       thread-last
		       %1
		       (symbol-name)
		       (format "%s-hook")
		       (intern)))
	     
	     (seq-do (## add-hook %1 #'eglot-ensure)))

;;;; Boosting
(straight-use-package
 '(eglot-booster
   :type git
   :host github
   :repo "jdtsmith/eglot-booster"))

(add-hook 'after-init-hook #'eglot-booster-mode)

;;; AI Integration

;;;; Aider
(straight-use-package 'aidermacs)

(setq aidermacs-backend 'vterm)

(keymap-set global-map
	    "C-c a"
	    #'aidermacs-transient-menu)

;;; Toolkit

;;;; File Explorer
(add-hook 'dired-mode-hook #'dired-hide-details-mode)
(add-hook 'dired-mode-hook #'dired-omit-mode)

(setq dired-kill-when-opening-new-dired-buffer t)

;;;; Git
(straight-use-package 'magit)

;;;; Terminal
(straight-use-package 'vterm)

;;; Extra
(setq inhibit-startup-screen t)
(setq make-backup-files nil)

(setq-default cursor-type 'bar)
