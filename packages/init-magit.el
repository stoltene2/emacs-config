(require 'magit)

;; Custom helpers
(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))


(global-set-key (kbd "C-c g") 'magit-status)
(define-key magit-status-mode-map (kbd "q") 'magit-quit-session)

;; Setup flyspell on gitcommits
(add-hook 'magit-commit-mode-hook (lambda ()
                                    (flyspell-mode)))

(setq projectile-switch-project-action (lambda ()
                                         (magit-status default-directory)))
