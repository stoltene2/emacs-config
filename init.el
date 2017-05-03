;; Allow this to function outside of an emacs.d directory
(setq debug-on-error t)

(if (not (boundp 'user-emacs-directory))
    (setq user-emacs-directory "~/.emacs.d/"))

(setenv "PATH" (concat "/usr/local/bin:"
                       (concat (getenv "HOME") "/.local/bin:")
                       (getenv "PATH")))


(setq exec-path (cons (concat (getenv "HOME") "/.local/bin") exec-path ))
(setq exec-path (cons "/usr/local/bin" exec-path ))

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             ;; The 't' means to append, so that MELPA comes after the more
             ;; stable ELPA archive.
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Switch to use-package

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; package configuration

(use-package ag
  :ensure t)

(use-package avy
  :ensure t
  :bind (("C-c SPC" . avy-goto-word-1)
         ("M-C-g" . avy-goto-line))

  :config
  (setq avy-background t))

(use-package bookmark+
  :ensure t)

(use-package company
  :diminish (company-mode . "\u24B8") ;; Circled C
  :ensure t
  :config
  (add-hook 'after-init-hook
            (lambda ()
              (progn
                (global-company-mode)
                (setq company-minimum-prefix-length 3)
                (setq company-tooltip-margin 1)
                (setq company-tooltip-minimum-width 30)))))

(use-package deft
  :ensure t
  :config
  (setq deft-extension "org")
  (setq deft-text-mode 'org-mode)
  (setq deft-directory "~/Documents/deft")
  (setq deft-use-filename-as-title t)
  (setq deft-auto-save-interval 0))

(use-package emmet-mode
  :ensure t
  :config
  (setq emmet-indentation 2)
  (add-hook 'web-mode-hook #'emmet-mode))

(use-package ensime
  :ensure t
  :commands ensime ensime-mode
  :config
  (add-hook 'scala-mode-hook 'ensime-mode)
  (setq exec-path (append exec-path '("/usr/local/bin"))))

(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)
         ("M-=" . er/contract-region)))

(use-package flycheck
  :ensure t
  :diminish (flycheck-mode . "\u24BB") ;; Circled F
  :bind (:map flycheck-mode-map
              ("C-c ! h" . helm-flycheck))

  :config
  (setq flycheck-disabled-checkers '(javascript-jshint json-jsonlist))
  (setq flycheck-checkers '(javascript-eslint))

  (flycheck-add-mode 'javascript-eslint 'js-mode)
  (flycheck-add-mode 'javascript-eslint 'js2-mode)

  (add-hook 'after-init-hook #'global-flycheck-mode))


(use-package git-gutter
  :ensure t
  :diminish git-gutter-mode
  :config
  (global-git-gutter-mode 1))

(use-package git-timemachine
  :ensure t)

(use-package haskell-mode
  :ensure t
  :config
  (setq exec-path
        (cons
         (concat (getenv "HOME") "/Library/Haskell/bin")
         exec-path))

  ;; Check for hindent on the system
  (require 'haskell-interactive-mode)
  (require 'haskell-process)

  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)

  (custom-set-variables
   '(haskell-process-args-cabal-repl '("--ghc-option=-ferror-spans"))
   '(haskell-process-auto-import-loaded-modules t)
   '(haskell-process-log t)
   '(haskell-process-suggest-remove-import-lines t)
   '(haskell-process-type 'stack-ghci)
   '(haskell-indentation-left-offset 4)
   '(haskell-indent-spaces 4)
;   '(haskell-tags-on-save t)
   '(haskell-process-use-presentation-mode t))

  (setq haskell-process-path-stack
        (concat (getenv "HOME")
                "/.local/bin/stack")))

(use-package helm
  :ensure t
  :diminish helm-mode
  :bind (("M-x" . helm-M-x)
         :map helm-map
         ([tab] . helm-execute-persistent-action)
         ("C-z" . helm-select-action))
  :config
  (global-unset-key (kbd "C-x c"))
  (setq helm-quick-update                     t
        helm-split-window-in-side-p           nil
        helm-buffers-fuzzy-matching           t
        helm-recentf-fuzzy-match              t
        helm-locate-fuzzy-match               t
        helm-M-x-fuzzy-match                  t
        helm-semantic-fuzzy-match             t
        helm-apropos-fuzzy-match              t
        helm-imenu-fuzzy-match                t
        helm-lisp-fuzzy-completion            t
        helm-move-to-line-cycle-in-source     t
        helm-scroll-amount                    8
        helm-ff-search-library-in-sexp        t
        helm-ff-file-name-history-use-recentf t)

  (helm-mode 1))

(use-package helm-ag
  :ensure t
  :config
  (eval-after-load 'helm
    '(progn
       (require 'helm-ag))))

(use-package helm-projectile
  :ensure t
  :config
  (setq helm-projectile-fuzzy-match t))

(use-package helm-swoop
  :ensure t)

;; Helper mode for emacs but requires emacs 24.5
;; http://commercialhaskell.github.io/intero/
(use-package intero
  :ensure t
  :config
  (add-hook 'haskell-mode-hook 'intero-mode))

(use-package jasminejs-mode
  :ensure t
  :diminish jasminejs-mode
  :config
  (add-hook 'jasminejs-mode-hook
            (lambda ()
              (jasminejs-add-snippets-to-yas-snippet-dirs)))

  (add-hook 'jasminejs-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c j") 'jasminejs-prefix-map)))

  (add-hook 'js2-mode-hook (lambda () (jasminejs-mode))))

(use-package jenkins
  :ensure t)

(use-package js2-mode
  :ensure t
  :mode ("\\.js$" . js2-mode)
  :diminish (js2-minor-mode . "\u24BF") ;; J
  :config
  (add-hook 'js2-mode-hook
            (lambda () (subword-mode)))

  ;; Setup company mode for js2-mode
  (add-hook 'js2-mode-hook
            (lambda ()
              (set (make-local-variable 'company-backends)
                   '((company-dabbrev-code company-yasnippet)))))

  (custom-set-variables
   '(js2-auto-insert-catch-block nil)
   '(js2-basic-offset 2)
   '(js2-bounce-indent-p nil)
   '(js2-mode-indent-ignore-first-tab nil))

  (eval-after-load 'js2-mode
    (progn (flycheck-mode))))

(use-package js2-refactor
  :ensure t
  :diminish js2-refactor-mode
  :config
  (add-hook 'js2-mode-hook #'js2-refactor-mode)
  (js2r-add-keybindings-with-prefix "C-c RET"))

(use-package json-mode
  :ensure t
  :config
  (add-hook 'json-mode-hook
            (lambda ()
              (setq js-indent-level 2))))

(use-package less-css-mode
  :ensure t
  :config
  (setq css-indent-offset 2))

(use-package magit
  :ensure t
  :diminish ((magit-mode . "") (magit-status-mode . ""))
  :bind (("C-c g" . magit-status)
         :map magit-status-mode-map
         ("q" . magit-quit-session))

  :config

  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))

  (defun magit-quit-session ()
    "Restores the previous window configuration and kills the magit buffer"
    (interactive)
    (kill-buffer)
    (jump-to-register :magit-fullscreen)))


(use-package markdown-mode
  :ensure t)

(use-package monokai-theme
  :ensure t)

(use-package multiple-cursors
  :ensure t
  :bind (("C-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

(use-package neotree
  :ensure t
  :bind ([f7] . neotree-toggle)
  :config
  (setq neo-window-width 50))

(use-package org-pomodoro
  :ensure t)

(use-package paredit
  :ensure t)

(use-package projectile
  :ensure t
  :diminish (projectile-mode . "\u24C5") ;; Ⓟ
  :config
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
                   '(setq projectile-switch-project-action #'magit-status))))))

(use-package puppet-mode
  :ensure t
  :mode ("\\.pp$" . puppet-mode))

(use-package rainbow-delimiters
  :ensure t)

(use-package restclient
  :ensure t)

(use-package ruby-mode
  :ensure t
  :mode (("\\.rb$" . ruby-mode)
         ("Gemfile" . ruby-mode)
         ("Rakefile" . ruby-mode)
         ("\\.rake$" . ruby-mode)))

(use-package shakespeare-mode
  :ensure t)

(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :config
  (require 'smartparens-config)
  (smartparens-global-mode t)
  (sp-use-paredit-bindings))


;; (use-package tide
;;   :ensure t
;;   :config
;;   (setq typescript-indent-level 2)
;;   (add-hook 'typescript-mode-hook
;;             (lambda ()
;;               (tide-setup)
;;               ;;(flycheck-mode +1)
;;               ;;(setq flycheck-check-syntax-automatically '(save mode-enabled))
;;               (eldoc-mode +1)
;;               ;; company is an optional dependency. You have to
;;               ;; install it separately via package-install
;;               (company-mode-on)
;;               (setq company-tooltip-align-annotations t)
;;               ))
;;   (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
;;   (add-hook 'web-mode-hook
;;             (lambda ()
;;               (when (string-equal "tsx" (file-name-extension buffer-file-name))
;;                 (tide-setup)
;;                 (flycheck-mode +1)
;;                 ;;(setq flycheck-check-syntax-automatically '(save mode-enabled))
;;                 (eldoc-mode +1)
;;                 (company-mode-on)))))

;; (use-package typescript-mode
;;   :ensure t
;;   :mode ("\\.ts\\'" . typescript-mode)
;;   :init (setq typescript-indent-level 2))

(use-package web-mode
  :ensure t
  :mode ("\\.html\\'" . web-mode)
  :config
  (setq web-mode-enable-current-column-highlight t)
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-markup-indent-offset 2))

(use-package yasnippet
  :ensure t
  :diminish (yas-minor-mode . "\u24CE")
  :demand t
  :config
  (yas-global-mode)
  (define-key yas-keymap (kbd "<return>") 'yas-next-field))

(use-package yatemplate
  :ensure t
  :demand t
  :init (auto-insert-mode)
  :config (yatemplate-fill-alist))

(use-package yaml-mode
  :ensure t
  :mode ("\\.yml" . yaml-mode))

;; Diminish included modes
(diminish 'auto-revert-mode)
(diminish 'subword-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load custom initialization for after packages have loaded

(message "My emacs directory is: %s" user-emacs-directory)

(mapcar (lambda (f)
          (message "loading %s" f)
          (load-file f))
        (file-expand-wildcards (concat user-emacs-directory "init/*.el")))

(setq debug-on-error nil)
