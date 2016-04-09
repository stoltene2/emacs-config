;; Helm Flycheck
(require 'flycheck)
(eval-after-load 'flycheck
  '(define-key flycheck-mode-map (kbd "C-c ! h") 'helm-flycheck))

(add-hook 'after-init-hook #'global-flycheck-mode)

(setq flycheck-disabled-checkers '(javascript-jshint json-jsonlist))
(setq flycheck-checkers '(javascript-eslint))

(flycheck-add-mode 'javascript-eslint 'js-mode)
(flycheck-add-mode 'javascript-eslint 'js2-mode)
