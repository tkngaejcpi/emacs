;; TODO: setup treesit

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

;;; Package Management

;;;; Bootstrap
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
(straight-use-package 'llama)
(straight-use-package 'dash)

(require 'llama)
(require 'dash)

;;; Appearance

;;;; Frame
(setq default-frame-alist
      '((height . 40)
	(width . 80)
	(alpha-background . 95)))

;;;; Bar
(add-hook 'after-init-hook
	  (lambda ()
	    (menu-bar-mode -1)
	    (tool-bar-mode -1)))

(set-window-scroll-bars (minibuffer-window) nil nil nil nil 1)

;;;; Theme
(straight-use-package 'modus-themes)

(add-hook 'after-init-hook
	  (## load-theme 'modus-operandi t))

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
(setq-default mode-line-format
	      (list
	       mode-line-front-space
	       
	       mode-line-buffer-identification
	       "  "
	       
	       '(:eval mode-name)

	       'mode-line-format-right-align
	       
	       '(:eval '(vc-mode vc-mode))
	       
	       mode-line-end-spaces))

;;; Completion

;;;; Minibuffer
(straight-use-package 'vertico)
(add-hook 'after-init-hook #'vertico-mode)

;;;; Buffer
(straight-use-package 'corfu)
(add-hook 'after-init-hook #'global-corfu-mode)

(setq corfu-auto t)

;;;; Completion Style
(straight-use-package 'orderless)

(setq completion-styles '(orderless basic))
(setq completion-category-overrides '((file (styles basic partial-completion))))

;;; Editing

;;;; Highlight Line
(add-hook 'after-init-hook #'global-hl-line-mode)

(dolist (hook '(vterm-mode-hook))
  (add-hook hook
	    (## hl-line-mode 'toggle)))

;;;; Line Number
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;;;; Repeat
(add-hook 'after-init-hook #'repeat-mode)

;;;; Jumping
(straight-use-package 'avy)

(keymap-set global-map
	    "C-."
	    #'avy-goto-char-timer)

;;;; Emacs Lisp

;;;;; Indent
(autoload #'common-lisp-indent-function "cl-indent")
(add-hook 'emacs-lisp-mode-hook
	  (## setq lisp-indent-function #'common-lisp-indent-function))

;;;;; Pair Edit
(straight-use-package 'paredit)
(add-hook 'emacs-lisp-mode-hook #'paredit-mode)

;;; Extra Modes

;;;; Markdown Mode
(straight-use-package 'markdown-mode)

;;; Language Server

;;;; Servers
(with-eval-after-load 'eglot
  (dolist (config
	    `(((python-mode python-ts-mode)
	       ,(executable-find "uvx")
	       "--from"
	       "basedpyright"
	       "basedpyright-langserver"
	       "--stdio")

	      (typescript-ts-mode
	       ,(executable-find "bunx")
	       "typescript-language-server"
	       "--stdio")))

    (add-to-list 'eglot-server-programs config)))

(dolist (hook '(python-mode-hook
		typescript-ts-mode))
  (add-hook hook #'eglot-ensure))

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

(setq mouse-wheel-follow-mouse t)
