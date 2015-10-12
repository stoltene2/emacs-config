(add-hook 'after-init-hook
          (lambda ()
            (progn
              (global-company-mode)
              (setq company-minimum-prefix-length 3)
              (setq company-tooltip-margin 1)
              (setq company-tooltip-minimum-width 30)
              (global-auto-complete-mode -1))))
