(require 'projectile)
(projectile-global-mode)
(setq projectile-completion-system 'helm)

(defun es/projectile-test-suffix (project-type)
  "This is the default ending for javascript test files"
  "-spec")


(custom-set-variables
 '(projectile-test-files-suffices (quote ("_test" "_spec" "Spec" "Test" "-test" "-spec")))
 '(projectile-test-suffix-function (quote es/projectile-test-suffix)))


(eval-after-load 'helm
  '(progn
     (require 'helm-projectile)
     (setq projectile-completion-system 'helm)
     (helm-projectile-on)
     (eval-after-load 'magit
       '(setq projectile-switch-project-action #'magit-status))))
