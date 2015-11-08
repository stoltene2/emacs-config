(global-git-gutter-mode 1)

(add-hook 'git-gutter-mode-hook
          (lambda ()
            (progn
              (local-set-key (kbd "M-n") 'git-gutter:next-hunk)
              (local-set-key (kbd "M-p") 'git-gutter:previous-hunk))))
