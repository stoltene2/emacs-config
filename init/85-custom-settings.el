(custom-set-variables
 '(safe-local-variable-values
   (quote
    ((projectile-test-suffix-function lambda
                                      (project-type)
                                      "" "Spec")
     (eval progn
           (require
            (quote projectile))
           (puthash
            (projectile-project-root)
            (concat haskell-process-path-stack " build")
            projectile-compilation-cmd-map)
           (puthash
            (projectile-project-root)
            (concat haskell-process-path-stack " test")
            projectile-test-cmd-map)))))

 '(compilation-ask-about-save nil)
 '(compilation-scroll-output (quote first-error))
 '(org-agenda-files
   (quote
    ("~/Documents/deft")))
 '(org-clock-clocktable-default-properties (quote (:maxlevel 3 :scope file)))
 '(org-clock-idle-time 15)
 '(org-clock-into-drawer "LOGBOOK")
 '(org-clock-out-remove-zero-time-clocks t)
 '(org-clocktable-defaults
   (quote
    (:maxlevel 3 :lang "en" :scope file :block nil :tstart nil :tend nil :step nil :stepskip0 nil :fileskip0 nil :tags nil :emphasize nil :link nil :narrow 40! :indent t :formula nil :timestamp nil :level nil :tcolumns nil :formatter nil)))
 '(org-enforce-todo-checkbox-dependencies t)
 '(org-enforce-todo-dependencies t)
 '(org-fontify-emphasized-text t)
 '(org-fontify-whole-heading-line t)
 '(org-src-fontify-natively t)
 '(org-habit-following-days 5)
 '(org-habit-show-habits-only-for-today t)
 '(org-habit-today-glyph 124)
 '(org-hide-emphasis-markers t)
 '(org-hide-leading-stars t)
 '(org-log-done (quote time))
 '(org-modules
   ())
 '(org-tags-column -120)
 '(org-todo-keyword-faces (quote (("TODO" . "#b58900") ("NEXT" . "#2aa198")))))


(custom-set-variables
 '(ediff-window-setup-function (quote ediff-setup-windows-plain)))
