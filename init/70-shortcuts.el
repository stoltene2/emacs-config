;; Quickly jump to a line
(global-set-key [(meta g)] 'goto-line)

(global-set-key [S-f8] 'compile)
(global-set-key [f8] 'recompile)

;; Quick switch to the last buffer
(global-set-key [backtab] (lambda ()
                            (interactive)
                            (switch-to-buffer (other-buffer))))

;; Not used a lot, but handy on new projects
(global-set-key [f5] 'speedbar-get-focus)

(global-set-key [(meta !)] 'async-shell-command)
(global-set-key [(control meta !)] 'shell-command)

(global-set-key (kbd "C-c r") 'rgrep)

(global-set-key (kbd "<C-return>") 'es/open-line-below)
(global-set-key (kbd "<C-S-return>") 'es/open-line-above)

(global-set-key [f9] 'es/toggle-window-split)
(global-set-key [f10] 'es/rotate-windows)


(js2r-add-keybindings-with-prefix "C-c RET")

(global-set-key (kbd "M-s s") 'helm-swoop)

(global-set-key [f5] 'helm-projectile-grep)

(global-set-key [f2] 'es/cycle-font-size)
