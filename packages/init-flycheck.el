;; Helm Flycheck
(require 'flycheck)
(eval-after-load 'flycheck
  '(define-key flycheck-mode-map (kbd "C-c ! h") 'helm-flycheck))

(add-hook 'after-init-hook #'global-flycheck-mode)


(custom-set-variables
 '(flycheck-jshintrc "~/config/jshintrc"))
