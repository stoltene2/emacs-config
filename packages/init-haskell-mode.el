;; To make this mode work you need to install some packages with
;; cabal first.

;; If you run into some cabal hell try clearing out the cached packages:

;; $ rm -rf `find ~/.ghc -maxdepth 1 -type d`
;; $ rm -rf ~/.cabal/lib
;; $ rm -rf ~/.cabal/packages
;; $ rm -rf ~/.cabal/share

;; $ cabal install alex happy
;; $ cabal install ghci-ng

(require 'haskell)
(require 'haskell-mode)

;; Check for hindent on the system
(require 'haskell-process)

(require 'haskell-interactive-mode)
(require 'haskell-font-lock)

(custom-set-variables
 '(haskell-process-type 'cabal-repl)
 '(haskell-process-args-cabal-repl
   '("--ghc-option=-ferror-spans"))
 '(haskell-notify-p t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-log t)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-use-presentation-mode t))


;;; Hooks
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

;;; Custom keys
(define-key interactive-haskell-mode-map (kbd "C-c '") 'haskell-interactive-bring)
