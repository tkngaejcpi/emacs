(straight-use-package 'orderless)

(setq completion-styles '(orderless basic))
(setq completion-category-overrides '((file (styles basic partial-completion))))

(provide 'init-orderless)
