;; DEPRECATED, but keep it in the codebase in case of ?

(defvar +autoload-directory (expand-file-name "autoload"
					      user-emacs-directory))


(defvar +autoload-modules-directory (expand-file-name "lisp/module"
						      user-emacs-directory))

(defun +autoload-get-autoload-file (package)
  (expand-file-name (format "%s-autoload.el"
			    (symbol-name package))
		    +autoload-directory))

(defun +autoload-get-module-directory (package)
  (expand-file-name (symbol-name package)
		    +autoload-modules-directory))

(defun +autoload-get-module-extensions-directory (package)
  (expand-file-name "extensions"
		    (+autoload-get-module-directory package)))

(defun +autoload-autoload-available-p (package)
  (file-exists-p (+autoload-get-autoload-file package)))

(defun +autoload-generate-autoload-file (package)
  (require 'loaddefs)

  (unless (file-directory-p +autoload-directory)
    (mkdir +autoload-directory))

  (let ((module-directory (+autoload-get-module-directory package))
        (module-extensions-directory (+autoload-get-module-extensions-directory package)))

    (loaddefs-generate (if (file-directory-p module-extensions-directory)
                           `(,module-extensions-directory
                             ,module-directory)

                         `(,module-directory))

                       (+autoload-get-autoload-file package))))

(defun +autoload-load-autoload-file (package)
  (+autoload-add-load-path +autoload-directory)
  (require (intern (format "%s-autoload" (symbol-name package)))))

(defun +autoload-add-load-path (path)
  (when (file-directory-p path)
    (unless (member path load-path)
      (add-to-list 'load-path path))))

(defun +autoload-setup (package)
  (+autoload-add-load-path (+autoload-get-module-directory package))
  (+autoload-add-load-path (+autoload-get-module-extensions-directory package))
  
  (unless (+autoload-autoload-available-p package)
    (+autoload-generate-autoload-file package))

  (+autoload-load-autoload-file package))

(provide '+autoload)
