;; To make this mode work you need to install some packages with
;; cabal first.

;; If you run into some cabal hell try clearing out the cached packages:

;; $ rm -rf `find ~/.ghc -maxdepth 1 -type d`
;; $ rm -rf ~/.cabal/lib
;; $ rm -rf ~/.cabal/packages
;; $ rm -rf ~/.cabal/share

;; $ cabal install alex happy
;; $ cabal install ghci-ng

(setq exec-path (cons (concat (getenv "HOME") "/Library/Haskell/bin") exec-path))

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
 '(haskell-tags-on-save t)
 ;; '(haskell-notify-p t)
 ;; '(haskell-process-suggest-remove-import-lines t)
 ;; '(haskell-process-log t)
 ;; '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-use-presentation-mode t))

;;; Hooks
;;; Custom keys
;; (define-key interactive-haskell-mode-map (kbd "C-c '") 'haskell-interactive-bring)


(setq haskell-process-path-stack "/Users/eric/Library/Haskell/bin/stack")
