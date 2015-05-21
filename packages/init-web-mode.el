(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))

(setq web-mode-enable-current-column-highlight t)
(setq web-mode-enable-current-element-highlight t)

(setq web-mode-markup-indent-offset 2)
