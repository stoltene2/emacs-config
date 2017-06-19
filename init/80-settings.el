;; This file is for overriding or configuring emacs settings

;; Save place mode
(if (/= 24 emacs-major-version)
    (save-place-mode 1)
  (progn
    (require 'saveplace)
    (setq-default save-place t)))

(ansi-color-for-comint-mode-on)

(defvar browse-url-generic-program)
(defvar browse-url-browser-function)

;; Get to the browser
(dolist (executable (list "google-chrome" "chromium-browser" "firefox"))
  (let ((browser-path (executable-find executable)))
    (when browser-path
      (setq browse-url-generic-program browser-path
            browse-url-browser-function 'browse-url-generic)
      (return browser-path))))

;; Bad tabs, bad.
(setq-default indent-tabs-mode nil)

;; Kill that trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq default-line-spacing 4)

(show-paren-mode t)

(custom-set-variables
 '(show-paren-style 'parenthesis))

(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)


;;; Dired customizations
(defun dired-back-to-top ()
  (interactive)
  (beginning-of-buffer)
  (dired-next-line 4))

(define-key dired-mode-map
  (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)

(defun dired-jump-to-bottom ()
  (interactive)
  (end-of-buffer)
  (dired-next-line -1))

(define-key dired-mode-map
  (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom)

;;; Backup
(custom-set-variables
 ;; don't clobber symlinks
 '(backup-by-copying t)
 ;; Don't litter
 '(backup-directory-alist '(("." . "~/.saves")))
 '(delete-old-versions t)
 '(kept-new-versions 6)
 '(kept-old-versions 2)
 '(version-control t)
 '(create-lockfiles nil))

;; IBuffer
(setq ibuffer-formats
 (quote
  ((mark modified read-only " "
         (name 60 60 :left :elide)
         " "
         (mode 14 14 :left :elide)
         " " filename-and-process)
   (mark " "
         (name 12 -1)
         " " filename))))
