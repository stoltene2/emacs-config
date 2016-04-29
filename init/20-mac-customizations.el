;; Put Mac specific configurations in here that are not related to the
;; theme.

(if (eq system-type 'darwin)
    (progn
      (require 'ls-lisp)
      (setq ls-lisp-use-insert-directory-program nil)

      (setq mac-command-modifier 'meta)

      (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
      (setq mouse-wheel-progressive-speed nil)            ;; don't accelerate scrolling
      (setq mouse-wheel-follow-mouse 't)                  ;; scroll window under mouse
      (setq scroll-step 1)                                ;; keyboard scroll one line at a time

      (global-set-key (kbd "M-`") 'other-frame)
      ;; The popup message box destroys the system
      (defadvice yes-or-no-p (around prevent-dialog activate)
        "Prevent yes-or-no-p from activating a dialog"
        (let ((use-dialog-box nil))
          ad-do-it))
      (defadvice y-or-n-p (around prevent-dialog-yorn activate)
        "Prevent y-or-n-p from activating a dialog"
        (let ((use-dialog-box nil))
          ad-do-it))
      (defadvice message-box (around prevent-dialog activate)
        "Prevent message-box from activating a dialog"
        (apply #'message (ad-get-args 0)))
      ))
