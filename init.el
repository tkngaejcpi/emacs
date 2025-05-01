;;; customize emacs
(setq custom-file
      (locate-user-emacs-file "custom.el"))

(load custom-file)

;;; bootstrap straight
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

;;; load config
(add-to-list 'load-path
	     (locate-user-emacs-file "lisp/config"))

(require 'init-aidermacs)
(require 'init-avy)
(require 'init-corfu)
(require 'init-dired)
(require 'init-display-line-numbers)
(require 'init-eglot)
(require 'init-eglot-booster)
(require 'init-emacs)
(require 'init-gptel)
(require 'init-hl-line)
(require 'init-magit)
(require 'init-markdown-mode)
(require 'init-minuet)
(require 'init-modus-themes)
(require 'init-orderless)
(require 'init-paredit)
(require 'init-persp-mode)
(require 'init-repeat)
(require 'init-tab-line)
(require 'init-treesit)
(require 'init-vertico)
(require 'init-vterm)
(require 'init-web-mode)

