(add-hook 'jasminejs-mode-hook (lambda () (jasminejs-add-snippets-to-yas-snippet-dirs)))

(add-hook 'jasminejs-mode-hook (lambda ()
                                 (local-set-key (kbd "C-c j") 'jasminejs-prefix-map)))

(add-hook 'js2-mode-hook (lambda () (jasminejs-mode)))
