;; Allow this to function outside of an emacs.d directory
(setq debug-on-error t)

;; Enable really large output from server processes
(setq read-process-output-max 8000000)

;; Set the gc threshold to be larger. We've got big enough machines now
(setq gc-cons-threshold 100000000)

(if (not (boundp 'user-emacs-directory))
    (setq user-emacs-directory "~/.emacs.d/"))


(let ((home (getenv "HOME"))
      (path (getenv "PATH")))
  (setenv "PATH" (concat "/usr/local/bin" ":"
                         (concat home "/.cargo/bin") ":"
                         (concat home "/.rbenv/shims") ":"
                         (concat home "/.local/bin") ":"
                         (concat home "/.nix-profile/bin") ":"
                         "/nix/var/nix/profiles/default/bin" ":"
                         path)))

(let ((home (getenv "HOME")))
  (progn
    (add-to-list 'exec-path (concat home "/.rbenv/shims"))
    (add-to-list 'exec-path (concat home "/.local/bin"))
    (add-to-list 'exec-path (concat home "/.cargo/bin"))
    (add-to-list 'exec-path (concat home "/.npm/bin"))
    (add-to-list 'exec-path (concat home "/.nix-profile/bin"))
    ;; If darwin
    (add-to-list 'exec-path "/nix/var/nix/profiles/default/bin")
    (add-to-list 'exec-path "/usr/local/bin" t)))

(add-to-list 'auto-mode-alist '("\\.js$" . js-mode))
(add-hook 'js-mode-hook
          (lambda ()
            (set (make-local-variable 'company-backends)
                 '((company-dabbrev-code company-yasnippet)))))

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(require 'package)
(setq use-package-verbose t)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Switch to use-package
;; https://github.com/jwiegley/use-package/blob/master/README.md

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(defun es/reload-config ()
  "Expects literate org file to exist in the default emacs directory at the root"
  (interactive)
  (let ((literate-config (concat user-emacs-directory "readme.org")))
    (org-babel-load-file literate-config)))

;; Not sure if this is needed
(setq max-lisp-eval-depth 2000)

(es/reload-config)

;; Fancy icons for use with neo-tree
;; M-x all-the-icons-install-fonts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load custom initialization for after packages have loaded

(message "My emacs directory is: %s" user-emacs-directory)

(setq debug-on-error nil)

(bind-key* "C-;" #'execute-extended-command)
(bind-key* "C-:" #'execute-extended-command-for-buffer)
(bind-key* "C-c ;" #'execute-extended-command)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(backup-by-copying t)
 '(backup-directory-alist '(("." . "~/.saves")))
 '(bmkp-last-as-first-bookmark-file (concat user-emacs-directory "bookmarks"))
 '(compilation-ask-about-save nil)
 '(compilation-scroll-output 'first-error)
 '(create-lockfiles nil)
 '(custom-safe-themes
   '("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476"
     "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088"
     default))
 '(delete-old-versions t)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(inf-elixir-base-command "/home/eric/workspace/book-app/.devenv/profile/bin/iex")
 '(inf-elixir-project-command
   "/home/eric/workspace/book-app/.devenv/profile/bin/iex -S /home/eric/workspace/book-app/.devenv/profile/bin/mix phx.server")
 '(js-indent-level 2 nil nil "Customized with use-package json-mode")
 '(js2-auto-insert-catch-block nil)
 '(js2-bounce-indent-p nil)
 '(js2-mode-indent-ignore-first-tab nil)
 '(kept-new-versions 6)
 '(kept-old-versions 2)
 '(magit-repository-directories '(("~/workspace" . 2)))
 '(org-agenda-files '("~/Documents/deft"))
 '(org-clock-clocktable-default-properties '(:maxlevel 3 :scope file))
 '(org-clock-idle-time 15)
 '(org-clock-into-drawer "LOGBOOK")
 '(org-clock-out-remove-zero-time-clocks t)
 '(org-clocktable-defaults
   '(:maxlevel 3 :lang "en" :scope file :block nil :tstart nil :tend nil
               :step nil :stepskip0 nil :fileskip0 nil :tags nil
               :emphasize nil :link nil :narrow 40! :indent t :formula
               nil :timestamp nil :level nil :tcolumns nil :formatter
               nil))
 '(org-enforce-todo-checkbox-dependencies t)
 '(org-enforce-todo-dependencies t)
 '(org-fontify-emphasized-text t)
 '(org-fontify-whole-heading-line t)
 '(org-habit-following-days 5)
 '(org-habit-show-habits-only-for-today t)
 '(org-habit-today-glyph 124)
 '(org-hide-emphasis-markers t)
 '(org-hide-leading-stars t)
 '(org-log-done 'time)
 '(org-modules nil)
 '(org-src-fontify-natively t)
 '(org-tags-column -120)
 '(org-todo-keyword-faces '(("TODO" . "#b58900") ("NEXT" . "#2aa198")))
 '(package-selected-packages
   '(all-the-icons-ivy auto-yasnippet cider company-box
                       counsel-projectile dap-mode default-text-scale
                       doom-themes elixir-mode emmet-mode envrc
                       expand-region fic-mode flycheck git-gutter
                       git-link git-timemachine go-mode haskell-mode
                       inf-elixir jasminejs-mode json-mode lsp-ui
                       magit major-mode-hydra mix multiple-cursors
                       nix-mode rainbow-delimiters restclient rg
                       rustic smartparens treemacs-projectile
                       typescript-mode undo-tree urlenc yatemplate
                       zig-mode))
 '(projectile-haskell-cabal-compile-cmd (concat haskell-process-path-stack " build"))
 '(projectile-haskell-cabal-test-cmd (concat haskell-process-path-stack " test"))
 '(rg-group-result t nil nil "Group the results by filename")
 '(safe-local-variable-values
   '((python-shell-interpreter . "docker")
     (python-shell-interpreter-args
      . "compose run -w /usr/src/app -i -v /var/folders/ns/:/var/folders/ns web /root/.cache/pypoetry/virtualenvs/parsely-VA82Wl8V-py3.9/bin/python")
     (python-shell-interpreter-interactive-arg . "-i")
     (lsp-pyright-venv-directory
      . "/Users/eric.stolten/Library/Caches/pypoetry/virtualenvs/parsely-ts4HD_Mz-py3.9")
     (projectile-test-suffix-function lambda (project-type) "" "Spec")
     (eval progn (require 'projectile)
           (puthash (projectile-project-root)
                    (concat haskell-process-path-stack " build")
                    projectile-compilation-cmd-map)
           (puthash (projectile-project-root)
                    (concat haskell-process-path-stack " test")
                    projectile-test-cmd-map))))
 '(show-paren-style 'parenthesis)
 '(tab-width 4 nil nil "Set from custom settings")
 '(version-control t)
 '(warning-suppress-types '(((python python-shell-completion-native-turn-on-maybe)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-annotation ((t (:inherit company-tooltip))))
 '(company-tooltip-scrollbar-thumb ((t (:background "#3b3b3b"))))
 '(company-tooltip-selection ((t (:background "#6b6b6b" :foreground "#BBF7EF"))))
 '(git-gutter:added ((t (:background "#276B22" :foreground "#276B22" :weight bold))))
 '(git-gutter:deleted ((t (:background "#592822" :foreground "#592822" :weight bold))))
 '(git-gutter:modified ((t (:background "#272888" :foreground "#272888" :weight bold)))))
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
