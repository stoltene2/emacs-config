(require 'projectile)
(projectile-global-mode)
(setq projectile-completion-system 'helm)

(defun es/projectile-test-suffix (project-type)
  "This is the default ending for javascript test files"
  "-spec")

(defun es/projectile-find-implementation-or-test-other-window ()
  "Toggle between the implementation and test in the other window"
  (interactive)
  (find-file-other-window (projectile-find-implementation-or-test (buffer-file-name))))

(custom-set-variables
 '(projectile-test-files-suffices
   '("_test" "_spec" "Spec" "Test" "-test" "-spec"))
 '(projectile-test-suffix-function #'es/projectile-test-suffix)
 '(projectile-haskell-cabal-test-cmd
   (concat haskell-process-path-stack " test"))
 '(projectile-haskell-cabal-compile-cmd
   (concat haskell-process-path-stack " build")))

(add-hook 'after-init-hook
          (lambda ()
            '(progn
               (require 'helm-projectile)
               (setq projectile-completion-system 'helm)
               (helm-projectile-on)
               (eval-after-load 'magit
                 '(setq projectile-switch-project-action #'magit-status)))))
