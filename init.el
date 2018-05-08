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
  :bind (("C-c SPC" . avy-goto-word-1))

  :config
  (setq avy-background t))

(use-package bazel-mode
  :ensure t
  :diminish bazel-mode)

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
                ;; Stop dabbrev from throwing case away
                (setq company-dabbrev-downcase nil)
                (setq company-minimum-prefix-length 3)
                (setq company-tooltip-margin 1)
                (setq company-tooltip-minimum-width 30)))))

(use-package default-text-scale
  :ensure t
  :config
  (setq default-text-scale-amount 8)
  :bind
  ([f2] . default-text-scale-increase)
  ([S-f2] . default-text-scale-decrease))

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


(use-package feature-mode)

(use-package fic-mode
  :ensure t

  :hook ((js2-mode-hook . fic-mode)
         (html-mode . fic-mode)
         (typescript-mode . fic-mode)))

(use-package flycheck
  :ensure t
  :diminish (flycheck-mode . "\u24BB") ;; Circled F
  :bind (:map flycheck-mode-map
              ("C-c ! h" . helm-flycheck))

  :config
  (setq flycheck-disabled-checkers '(javascript-jshint json-jsonlist typescript-tide))
  (setq flycheck-checkers '(javascript-eslint typescript-tslint))

  (flycheck-add-mode 'javascript-eslint 'js-mode)
  (flycheck-add-mode 'javascript-eslint 'js2-mode)

  (add-hook 'after-init-hook #'global-flycheck-mode))


;; Scheme related mode for Racket development
(use-package geiser
  :ensure t
  :config
  (add-hook 'geiser-mode-hook 'rainbow-delimiters-mode))

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

(use-package helm-dash
  :ensure t)

(use-package helm-projectile
  :ensure t
  :init
  (setq helm-projectile-fuzzy-match t))

(use-package helm-swoop
  :ensure t)

(use-package idris-mode)

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
  (add-hook 'js2-mode-hook #'hs-minor-mode)
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
  :bind ([f7] . neotree-toggle)
  :config
  (setq neo-window-width 50))

(use-package org-pomodoro)

(use-package paredit
  :ensure t)

;; pomadoro mode
;;(use-package pomidor)

(use-package projectile
  :ensure t
  :diminish (projectile-mode . "\u24C5") ;; Ⓟ

  :bind (:map projectile-mode-map
              ("C-c p s r" . rg-project))

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
     (concat haskell-process-path-stack " build"))
   '(haskell-indent-spaces 2)
   '(haskell-indentation-left-offset 2))

  (add-hook 'after-init-hook
            (lambda ()
              '(progn
                 (require 'helm-projectile)
                 (setq projectile-completion-system 'helm)
                 (helm-projectile-on)
                 (eval-after-load 'magit
                   '(setq projectile-switch-project-action #'magit-status))))))

(use-package psc-ide
  :ensure t
  :after purescript-mode

  :config
  (add-hook 'purescript-mode-hook
            (lambda ()
              (psc-ide-mode)
              (company-mode)
              (flycheck-mode)
              (turn-on-purescript-indentation))))

(use-package purescript-mode
  :ensure t)

(use-package rainbow-delimiters
  :ensure t)

(use-package restclient)

(use-package rg
  :ensure t
  :custom
  (rg-group-result t "Group the results by filename"))


(use-package ruby-mode
  :mode (("\\.rb$" . ruby-mode)
         ("Gemfile" . ruby-mode)
         ("Rakefile" . ruby-mode)
         ("\\.rake$" . ruby-mode)))

(use-package sass-mode)

(use-package shakespeare-mode)


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

(use-package tide
  :ensure t
  :config

  ;; Highlight identifier at points

  (defface font-lock-keyword-typescript-face
    '((t :foreground "SlateBlue1"))
    "My custom face for typescript keywords"
    :group 'font-lock-faces)

  (add-hook 'typescript-mode-hook
            (lambda ()
              (tide-setup)
              ;;(flycheck-mode +1)
              ;;(setq flycheck-check-syntax-automatically '(save mode-enabled))
              (eldoc-mode +1)
              ;; company is an optional dependency. You have to
              ;; install it separately via package-install
              (company-mode-on)
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
  (setq undo-tree-history-directory-alist `((".*" . ,(locate-user-emacs-file ".undo-tree"))))
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-relative-timestamps t)
  (global-undo-tree-mode))

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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(backup-by-copying t)
 '(backup-directory-alist (quote (("." . "~/.saves"))))
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
 '(compilation-ask-about-save nil)
 '(compilation-scroll-output (quote first-error))
 '(create-lockfiles nil)
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
    (psci psc-ide org sass-mode urlenc undo-tree yatemplate yaml-mode web-mode use-package tide sr-speedbar spacemacs-theme smartparens shakespeare-mode restclient rainbow-delimiters puppet-mode paredit org-pomodoro neotree monokai-theme markdown-mode magit less-css-mode json-mode js2-refactor jenkins jasminejs-mode intero idris-mode helm-swoop helm-projectile helm-ag git-timemachine git-gutter fic-mode feature-mode expand-region ensime emmet-mode dumb-jump deft default-text-scale bookmark+ avy ag)))
 '(projectile-haskell-cabal-compile-cmd (concat haskell-process-path-stack " build"))
 '(projectile-haskell-cabal-test-cmd (concat haskell-process-path-stack " test"))
 '(projectile-test-files-suffices (quote ("_test" "_spec" "Spec" "Test" "-test" "-spec")))
 '(projectile-test-suffix-function (function es/projectile-test-suffix))
 '(safe-local-variable-values
   (quote
    ((intero-targets "MiniMathematicians:lib" "MiniMathematicians:test:Tests")
     (intero-targets "MiniMathematicians:lib" "MiniMathematicians:exe:mini-web" "MiniMathematicians:test:Tests")
     (haskell-process-use-ghci . t)
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
