;; Allow this to function outside of an emacs.d directory
(setq debug-on-error t)

(if (not (boundp 'user-emacs-directory))
    (setq user-emacs-directory "~/.emacs.d/"))

(setenv "PATH" (concat "/usr/local/bin:"
                       (getenv "PATH")))

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

;; ;; Installed via el-get now
(use-package jasminejs-mode
  :ensure t)

(use-package js2-mode
  :ensure t)

(use-package js2-refactor
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
  :ensure t)

(use-package deft
  :ensure t
  :config
  (setq deft-extension "org")
  (setq deft-text-mode 'org-mode)
  (setq deft-directory "~/Documents/deft")
  (setq deft-use-filename-as-title t)
  (setq deft-auto-save-interval 0))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package flycheck
  :ensure t)

(use-package git-gutter
  :ensure t)


(use-package haskell-mode
  :ensure t)

(use-package helm
  :ensure t)

(use-package helm-ag
  :ensure t)

(use-package helm-projectile
  :ensure t)

(use-package helm-swoop
  :ensure t)

(use-package jenkins
  :ensure t)

(use-package json-mode
  :ensure t)

(use-package magit
  :ensure t
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
  :ensure t)

(use-package neotree
  :ensure t
  :bind ([f7] . neotree-toggle)
  :config
  (setq neo-window-width 50))

(use-package paredit
  :ensure t)

(use-package projectile
  :ensure t)

(use-package puppet-mode
  :ensure t)

(use-package restclient
  :ensure t)

(use-package ruby-mode
  :ensure t)

(use-package smartparens
  :ensure t)

(use-package web-mode
  :ensure t)

(use-package yasnippet
  :ensure t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load custom initialization for after packages have loaded

(message "My emacs directory is: %s" user-emacs-directory)

(mapcar (lambda (f)
          (message "loading %s" f)
          (load-file f))
        (file-expand-wildcards (concat user-emacs-directory "init/*.el")))

;; (setq debug-on-error nil)
;; (put 'narrow-to-region 'disabled nil)

;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(compilation-ask-about-save nil)
;;  '(compilation-scroll-output (quote first-error))
;;  '(ediff-window-setup-function (quote ediff-setup-windows-plain))
;;  '(elfeed-feeds
;;    (quote
;;     ("http://www.theminimalists.com/feed/"
;;      ("http://nullprogram.com/feed/" blog emacs)
;;      ("http://nedroid.com/feed/" webcomic)
;;      ("http://feeds.feedburner.com/brainpickings/rss" learning)
;;      ("http://xkcd.com/atom.xml" webcomic))))
;;  '(haskell-process-args-cabal-repl (quote ("--ghc-option=-ferror-spans")))
;;  '(haskell-process-auto-import-loaded-modules t)
;;  '(haskell-process-log t)
;;  '(haskell-process-suggest-remove-import-lines t)
;;  '(haskell-process-type (quote stack-ghci))
;;  '(haskell-process-use-presentation-mode t)
;;  '(haskell-tags-on-save t)
;;  '(js2-auto-insert-catch-block nil)
;;  '(js2-basic-offset 2)
;;  '(js2-bounce-indent-p nil)
;;  '(js2-mode-indent-ignore-first-tab nil)
;;  '(org-agenda-files (quote ("~/Documents/deft")))
;;  '(org-clock-clocktable-default-properties (quote (:maxlevel 3 :scope file)))
;;  '(org-clock-idle-time 15)
;;  '(org-clock-into-drawer "LOGBOOK")
;;  '(org-clock-out-remove-zero-time-clocks t)
;;  '(org-clocktable-defaults
;;    (quote
;;     (:maxlevel 3 :lang "en" :scope file :block nil :tstart nil :tend nil :step nil :stepskip0 nil :fileskip0 nil :tags nil :emphasize nil :link nil :narrow 40! :indent t :formula nil :timestamp nil :level nil :tcolumns nil :formatter nil)))
;;  '(org-enforce-todo-checkbox-dependencies t)
;;  '(org-enforce-todo-dependencies t)
;;  '(org-fontify-emphasized-text t)
;;  '(org-fontify-whole-heading-line t)
;;  '(org-habit-following-days 5)
;;  '(org-habit-show-habits-only-for-today t)
;;  '(org-habit-today-glyph 124)
;;  '(org-hide-emphasis-markers t)
;;  '(org-hide-leading-stars t)
;;  '(org-log-done (quote time))
;;  '(org-modules nil)
;;  '(org-src-fontify-natively t)
;;  '(org-tags-column -120)
;;  '(org-todo-keyword-faces (quote (("TODO" . "#b58900") ("NEXT" . "#2aa198"))))
;;  '(projectile-haskell-cabal-compile-cmd (concat haskell-process-path-stack " build") t)
;;  '(projectile-haskell-cabal-test-cmd (concat haskell-process-path-stack " test") t)
;;  '(projectile-test-files-suffices (quote ("_test" "_spec" "Spec" "Test" "-test" "-spec")))
;;  '(projectile-test-suffix-function (function es/projectile-test-suffix))
;;  '(safe-local-variable-values
;;    (quote
;;     ((projectile-test-suffix-function lambda
;;                                       (project-type)
;;                                       "" "Spec")
;;      (eval progn
;;            (require
;;             (quote projectile))
;;            (puthash
;;             (projectile-project-root)
;;             (concat haskell-process-path-stack " build")
;;             projectile-compilation-cmd-map)
;;            (puthash
;;             (projectile-project-root)
;;             (concat haskell-process-path-stack " test")
;;             projectile-test-cmd-map))))))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(company-scrollbar-fg ((t (:background "#3b3b3b"))))
;;  '(company-tooltip-annotation ((t (:inherit company-tooltip))))
;;  '(company-tooltip-selection ((t (:background "#6b6b6b" :foreground "#BBF7EF"))))
;;  '(git-gutter:added ((t (:background "#276B22" :foreground "#276B22" :weight bold))))
;;  '(git-gutter:deleted ((t (:background "#592822" :foreground "#592822" :weight bold))))
;;  '(git-gutter:modified ((t (:background "#272888" :foreground "#272888" :weight bold)))))
