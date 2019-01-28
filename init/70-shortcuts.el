;; Quickly jump to a line
(global-set-key [(meta g)] 'goto-line)

;; (global-set-key [S-f8] 'compile)
;; (global-set-key [f8] 'recompile)

;; Quick switch to the last buffer
(global-set-key [backtab] (lambda ()
                            (interactive)
                            (switch-to-buffer (other-buffer))))

(global-set-key [(meta !)] 'async-shell-command)
(global-set-key [(control meta !)] 'shell-command)

(global-set-key (kbd "C-c r") 'rgrep)
(global-set-key (kbd "C-c d") 'es/find-template-other-window)
(global-set-key (kbd "C-c c") 'es/collapse-all-functions)

(global-set-key (kbd "<C-return>") 'es/open-line-below)
(global-set-key (kbd "<C-S-return>") 'es/open-line-above)

(global-set-key [f9] 'es/toggle-window-split)
(global-set-key [f10] 'es/rotate-windows)

(global-set-key (kbd "<s-mouse-1>") 'hs-toggle-hiding)


(global-set-key [f5] 'helm-do-ag)
(global-set-key [S-f5] 'helm-swoop)

(global-set-key [f1] 'delete-other-windows)
(global-set-key [S-f1] 'delete-window)

(global-set-key (kbd "C-c C-d") #'es/comment-and-copy-line)

;; Org-mode
(global-set-key (kbd "C-c l") #'org-store-link)
