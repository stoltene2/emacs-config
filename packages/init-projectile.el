(require 'projectile)
(projectile-global-mode)
(setq projectile-completion-system 'helm)

(defun es/projectile-test-suffix (project-type)
  "Return test files of Spec for haskell-cabal projects
Use -spec for all other project types"
  (if (eq project-type 'haskell-cabal)
      "Spec"
    "-spec"))

(custom-set-variables
 '(projectile-test-files-suffices
   '("_test" "_spec" "Spec" "Test" "-test" "-spec"))
 '(projectile-test-suffix-function #'es/projectile-test-suffix)
 '(projectile-haskell-cabal-test-cmd
   (concat haskell-process-path-stack " test"))
 '(projectile-haskell-cabal-compile-cmd
   (concat haskell-process-path-stack " build")))

(eval-after-load 'helm
  '(progn
     (require 'helm-projectile)
     (setq projectile-completion-system 'helm)
     (helm-projectile-on)))
