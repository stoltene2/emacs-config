;; Helm Flycheck
(require 'flycheck)
(eval-after-load 'flycheck
  '(define-key flycheck-mode-map (kbd "C-c ! h") 'helm-flycheck))

(require 'flycheck)
(add-hook 'js-mode-hook
          (lambda () (flycheck-mode t)))
