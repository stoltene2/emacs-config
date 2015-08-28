(require 'js2-mode)
(autoload 'js2-mode "js2-mode" "Major mode for editing js files")
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(add-hook 'js2-mode-hook (lambda ()
                           (subword-mode)))

;; Setup company mode for js2-mode
(add-hook 'js2-mode-hook
            (lambda ()
              (set (make-local-variable 'company-backends)
                   '((company-dabbrev-code company-yasnippet)))))

(custom-set-variables
 '(js2-auto-insert-catch-block nil)
 '(js2-basic-offset 2)
 '(js2-bounce-indent-p nil)
 '(js2-mode-indent-ignore-first-tab nil))

(eval-after-load 'js2-mode
  (progn (flycheck-mode)))
