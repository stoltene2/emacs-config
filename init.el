;; Allow this to function outside of an emacs.d directory
(setq debug-on-error t)

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(if (not (boundp 'user-emacs-directory))
    (setq user-emacs-directory "~/.emacs.d/"))

(setenv "PATH" (concat "/usr/local/bin" ":"
                       (concat (getenv "HOME") "/.rbenv/shims") ":"
                       (concat (getenv "HOME") "/.local/bin") ":"
                       (getenv "PATH")))

(add-to-list 'exec-path (concat (getenv "HOME") "/.rbenv/shims"))
(add-to-list 'exec-path (concat (getenv "HOME") "/.local/bin"))
(add-to-list 'exec-path "/usr/local/bin" t)

(add-to-list 'auto-mode-alist '("\\.js$" . js-mode))
(add-hook 'js-mode-hook
          (lambda ()
            (set (make-local-variable 'company-backends)
                 '((company-dabbrev-code company-yasnippet)))))

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             ;; The 't' means to append, so that MELPA comes after the more
             ;; stable ELPA archive.
             '("melpa" . "http://melpa.org/packages/") t)

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

;; Fancy icons for use with neo-tree
;; M-x all-the-icons-install-fonts
(use-package all-the-icons
  :ensure t)

(use-package auto-yasnippet
  :ensure t)

;; https://github.com/abo-abo/auto-yasnippet
(use-package avy
  :ensure t
  :bind (("C-c SPC" . avy-goto-word-1))

  :config
  (setq avy-background t))

(use-package bazel-mode
  :ensure t
  :diminish bazel-mode)

(use-package bookmark+
  :disabled
  :ensure t)

(use-package company
  :diminish (company-mode . "\u24B8") ;; Circled C
  :ensure t
  :config
  (add-hook 'after-init-hook
            (lambda ()
              (progn
                (global-company-mode)
                ;; Stop dabbrev from throwing case away
                (setq company-dabbrev-downcase nil)
                (setq company-minimum-prefix-length 3)
                (setq company-tooltip-margin 1)
                (setq company-tooltip-minimum-width 30))))
  :bind
  (("C-'" . company-complete)))

(use-package default-text-scale
  :ensure t
  :config
  (setq default-text-scale-amount 8)
  :bind
  ;; Plus makes it better
  ("M-+" . default-text-scale-increase)
  ;; Underscore makes it smaller (- is already bound)
  ("M-_" . default-text-scale-decrease))

(use-package deft
  :ensure t
  :config
  (setq deft-extension "org")
  (setq deft-text-mode 'org-mode)
  (setq deft-directory "~/Documents/deft")
  (setq deft-use-filename-as-title t)
  (setq deft-auto-save-interval 0))

(use-package dumb-jump
  :ensure t
  :bind
  ("C-M-g" . dumb-jump-go))

(use-package emmet-mode
  :ensure t
  :config
  (setq emmet-indentation 2)
  (add-hook 'web-mode-hook #'emmet-mode))

(use-package ensime
  :commands ensime ensime-mode
  :config
  (add-hook 'scala-mode-hook 'ensime-mode)
  (setq exec-path (append exec-path '("/usr/local/bin"))))

(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)
         ("M-=" . er/contract-region)))


(use-package fic-mode
  :ensure t

  :hook ((js2-mode-hook . fic-mode)
         (html-mode . fic-mode)
         (ruby-mode . fic-mode)
         (js-mode . fic-mode)
         (typescript-mode . fic-mode)))

(use-package flycheck
  :ensure t
  :diminish (flycheck-mode . "\u24BB") ;; Circled F
  :bind (:map flycheck-mode-map
              ([f8] . flycheck-next-error)
              ([S-f8] . flycheck-list-errors))

  :config
  (setq flycheck-disabled-checkers '(javascript-jshint json-jsonlist typescript-tide))
  (setq flycheck-checkers '(javascript-eslint typescript-tslint))

  (flycheck-add-mode 'javascript-eslint 'js-mode)
;;  (flycheck-add-mode 'javascript-eslint 'js2-mode)

  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package forge
  :after magit)

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

  (custom-set-variables
   '(haskell-indentation-left-offset 4)
   '(haskell-indent-spaces 4)))

;; Helper mode for emacs but requires emacs 24.5
;; http://commercialhaskell.github.io/intero/
;; (use-package intero
;;   :config
;;   (add-hook 'haskell-mode-hook 'intero-mode))

(use-package ivy
  :ensure t
  ;; :init
  ;; (some-init-that-must-always-succeed)

  :config
  (ivy-mode 1)
  (setq ivy-height 16)

  :bind (("C-s" . swiper)))

(use-package counsel
  :ensure t)

(use-package counsel-projectile
  :ensure t)

(use-package jasminejs-mode
  :ensure t
  :diminish jasminejs-mode
  :config
  ;; This can be done differently
  (add-hook 'jasminejs-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c j") 'jasminejs-prefix-map)))

  (add-hook 'js2-mode-hook #'jasminejs-mode)
  (add-hook 'js-mode-hook #'jasminejs-mode)
  (add-hook 'typescript-mode-hook #'jasminejs-mode))

;; (use-package js2-mode
;;   :ensure t
;;   :mode ("\\.js$" . js2-mode)
;;   :diminish (js2-minor-mode . "\u24BF") ;; J
;;   :config
;;   (add-hook 'js2-mode-hook
;;             (lambda () (subword-mode)))
;;   (add-hook 'js2-mode-hook #'hs-minor-mode)
;;   ;; Setup company mode for js2-mode
;;   (add-hook 'js2-mode-hook
;;             (lambda ()
;;               (set (make-local-variable 'company-backends)
;;                    '((company-dabbrev-code company-yasnippet)))))

;;   (custom-set-variables
;;    '(js2-auto-insert-catch-block nil)
;;    '(js2-basic-offset 2)
;;    '(js2-bounce-indent-p nil)
;;    '(js2-mode-indent-ignore-first-tab nil))

;;   (eval-after-load 'js2-mode
;;     (progn (flycheck-mode))))

(use-package js2-refactor
  :after (js2-mode)
  :diminish js2-refactor-mode
  :config
  (js2r-add-keybindings-with-prefix "C-c RET"))

(use-package json-mode
  :ensure t
  :config
  (add-hook 'json-mode-hook #'hs-minor-mode)
  (add-hook 'json-mode-hook
            (lambda ()
              (setq js-indent-level 2))))

;; Needs emacs 25
;; (use-package json-navigator
;;   :ensure t)

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
    (magit-save-window-configuration)
    ad-do-it
    (delete-other-windows))

  (defun magit-quit-session ()
    "Restores the previous window configuration and kills the magit buffer"
    (interactive)
    (magit-restore-window-configuration)))

(use-package major-mode-hydra
  :ensure t
  :bind
  ("C-M-m" . major-mode-hydra))

(use-package markdown-mode
  :ensure t)


(use-package multiple-cursors
  :ensure t
  :bind (("C-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

(use-package neotree
  :ensure t
  :after (all-the-icons)
  :bind
  (([f7] . neotree-toggle)
   :map neotree-mode-map
   ("^" . es/neotree-dir-up))
  :config
  (setq neo-window-width 50)
  (setq neo-theme 'icons))

(use-package org-pomodoro
  :ensure t)

(use-package paredit
  :ensure t)

(use-package projectile
  ;; https://docs.projectile.mx/en/latest/
  :ensure t
  :diminish (projectile-mode . "\u24C5") ;; Ⓟ
  :bind (:map projectile-mode-map
              ("C-c p" . 'projectile-command-map)
              ("s-p" . 'projectile-command-map)

         :map projectile-command-map
              ("s r" . rg-project))

  :config
  (projectile-mode 1)
  (counsel-projectile-mode 1)
  (setq projectile-completion-system 'ivy)
  (setq projectile-switch-project-action #'magit-status)
  (setq projectile-generic-command "fd . -0")
  (defun es/projectile-test-suffix (project-type)
    "This is the default ending for javascript test files"
    "-spec")

  (defun es/projectile-find-implementation-or-test-other-window ()
    "Toggle between the implementation and test in the other window"
    (interactive)
    (find-file-other-window (projectile-find-implementation-or-test (buffer-file-name))))

  ;; These should be setq'd
  (custom-set-variables
   '(projectile-test-files-suffices
     '("_test" "_spec" "Spec" "Test" "-test" "-spec" ".spec"))
   '(projectile-test-suffix-function #'es/projectile-test-suffix)
   '(projectile-haskell-cabal-test-cmd
     (concat haskell-process-path-stack " test"))
   '(projectile-haskell-cabal-compile-cmd
     (concat haskell-process-path-stack " build"))
   '(haskell-indent-spaces 2)
   '(haskell-indentation-left-offset 2))

  (add-hook 'after-init-hook
            (lambda ()
              '(progn
                 (eval-after-load 'magit
                   '(setq projectile-switch-project-action #'magit-status))))))

(use-package projectile-rails
  ;; https://github.com/asok/projectile-rails
  :after (projectile)
  :config
  (projectile-rails-global-mode)
  :bind (:map projectile-rails-mode-map
              ("s-r" . 'hydra-projectile-rails/body)))


;; TODO: add to lisp, clojure, etc
(use-package rainbow-delimiters)

(use-package restclient
  :ensure t)

(use-package rg
  :ensure t
  :custom
  (rg-group-result t "Group the results by filename"))

(use-package ruby-mode
  :mode (("\\.rb$" . ruby-mode)
         ("Gemfile" . ruby-mode)
         ("Rakefile" . ruby-mode)
         ("\\.rake$" . ruby-mode)))

(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :config
  (require 'smartparens-config)
  (smartparens-global-mode t)
  (sp-use-paredit-bindings))


(use-package spacemacs-common
  :ensure spacemacs-theme
  :config (load-theme 'spacemacs-dark t))

(defun es/typescript-mode-extra-font-locks ()
  (font-lock-add-keywords nil
                          (list
                           '("\\<\\(constructor\\|type\\|declare\\|var\\|interface\\|static\\|public\\|private\\|this\\|implements\\|let\\|function\\|const\\|new\\|false\\|true\\)\\>"  1 'font-lock-keyword-typescript-face prepend))))

(use-package tide
  :bind
  (:map tide-mode-map
        ([f2] . tide-rename-symbol))

  :config
  (setq tide-completion-enable-autoimport-suggestions t)

  ;; Highlight identifier at points
  (defface font-lock-keyword-typescript-face
    '((t :foreground "SlateBlue1"))
    "My custom face for typescript keywords"
    :group 'font-lock-faces)

  (add-hook 'typescript-mode-hook
            (lambda ()
              (interactive)
              (tide-setup)
              (flycheck-mode +1)
              (setq flycheck-check-syntax-automatically '(save mode-enabled))
              (eldoc-mode +1)
              ;; company is an optional dependency. You have to
              ;; install it separately via package-install
              (company-mode +1)
              (tide-hl-identifier-mode +1)
              (setq company-tooltip-align-annotations t)
              (font-lock-add-keywords nil
                                      (list
                                       '("\\<\\(constructor\\|type\\|declare\\|var\\|interface\\|static\\|public\\|private\\|this\\|implements\\|let\\|function\\|const\\|new\\|false\\|true\\)\\>"  1 'font-lock-keyword-typescript-face prepend)))))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode)))



(use-package typescript-mode
  :ensure t
  :mode ("\\.ts\\'" . typescript-mode)
  :init (setq typescript-indent-level 2)
  :config
  (add-hook 'flycheck-mode-hook #'es/use-tslint-from-node-modules)
  (add-hook 'typescript-mode-hook #'hs-minor-mode)
  (add-hook 'typescript-mode-hook #'subword-mode))

(use-package undo-tree
  :ensure t
  :commands global-undo-tree-mode
  :config
  (global-undo-tree-mode 1)
  (setq undo-tree-history-directory-alist `((".*" . ,(locate-user-emacs-file ".undo-tree"))))
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-relative-timestamps t))


(use-package urlenc
  :ensure t)

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
  :mode ("\\.yml" . yaml-mode))

;; Diminish included modes
;;(diminish 'auto-revert-mode)
;;(diminish 'subword-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load custom initialization for after packages have loaded

(message "My emacs directory is: %s" user-emacs-directory)

(mapcar (lambda (f)
          (message "loading %s" f)
          (load-file f))
        (file-expand-wildcards (concat user-emacs-directory "init/*.el")))

(setq debug-on-error nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(backup-by-copying t)
 '(backup-directory-alist (quote (("." . "~/.saves"))))
 '(bmkp-last-as-first-bookmark-file "/Users/stoltene/.emacs.d/bookmarks")
 '(compilation-ask-about-save nil)
 '(compilation-scroll-output (quote first-error))
 '(create-lockfiles nil)
 '(custom-safe-themes
   (quote
    ("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" default)))
 '(delete-old-versions t)
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(haskell-indent-spaces 2)
 '(haskell-indentation-left-offset 2)
 '(haskell-process-args-cabal-repl (quote ("--ghc-option=-ferror-spans")))
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-type (quote stack-ghci))
 '(haskell-process-use-presentation-mode t)
 '(jenkins-api-token "4c735bff78fcb014d7d7c0f716ac0fb4")
 '(jenkins-url "http://http://cdlrdbxdvmstr01:8080/jenkins/")
 '(jenkins-username "stoltene")
 '(jenkins-viewname "Redbox SPA")
 '(js-indent-level 2)
 '(js2-auto-insert-catch-block nil)
 '(js2-bounce-indent-p nil)
 '(js2-mode-indent-ignore-first-tab nil)
 '(kept-new-versions 6)
 '(kept-old-versions 2)
 '(org-agenda-files (quote ("~/Documents/deft")))
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
 '(org-habit-following-days 5)
 '(org-habit-show-habits-only-for-today t)
 '(org-habit-today-glyph 124)
 '(org-hide-emphasis-markers t)
 '(org-hide-leading-stars t)
 '(org-log-done (quote time))
 '(org-modules nil)
 '(org-src-fontify-natively t)
 '(org-tags-column -120)
 '(org-todo-keyword-faces (quote (("TODO" . "#b58900") ("NEXT" . "#2aa198"))))
 '(package-selected-packages
   (quote
    (nix-mode rust-mode gnu-elpa-keyring-update forge magithub projectile-rails flow-js2-mode counsel-projectile ivy-hydra counsel ivy helm-cider cider 2048-game helm-c-yasnippet clojure-cheatsheet clojure-mode-extra-font-locking clojure-mode origami helm-rg all-the-icons auto-yasnippet gh-md urlenc undo-tree yatemplate yaml-mode web-mode use-package tide sr-speedbar spacemacs-theme smartparens shakespeare-mode restclient rainbow-delimiters puppet-mode paredit org-pomodoro neotree monokai-theme markdown-mode magit less-css-mode json-mode js2-refactor jenkins jasminejs-mode intero idris-mode helm-swoop helm-projectile helm-ag git-timemachine git-gutter fic-mode feature-mode expand-region ensime emmet-mode dumb-jump deft default-text-scale bookmark+ avy ag)))
 '(projectile-haskell-cabal-compile-cmd (concat haskell-process-path-stack " build"))
 '(projectile-haskell-cabal-test-cmd (concat haskell-process-path-stack " test"))
 '(projectile-test-files-suffices
   (quote
    ("_test" "_spec" "Spec" "Test" "-test" "-spec" ".spec")))
 '(projectile-test-suffix-function (function es/projectile-test-suffix))
 '(rg-group-result t)
 '(safe-local-variable-values
   (quote
    ((haskell-process-use-ghci . t)
     (haskell-indent-spaces . 4)
     (projectile-test-suffix-function lambda
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
 '(show-paren-style (quote parenthesis))
 '(version-control t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-scrollbar-fg ((t (:background "#3b3b3b"))))
 '(company-tooltip-annotation ((t (:inherit company-tooltip))))
 '(company-tooltip-selection ((t (:background "#6b6b6b" :foreground "#BBF7EF"))))
 '(git-gutter:added ((t (:background "#276B22" :foreground "#276B22" :weight bold))))
 '(git-gutter:deleted ((t (:background "#592822" :foreground "#592822" :weight bold))))
 '(git-gutter:modified ((t (:background "#272888" :foreground "#272888" :weight bold)))))
(put 'narrow-to-region 'disabled nil)
