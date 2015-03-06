;; This file is for overriding or configuring emacs settings

(ansi-color-for-comint-mode-on)

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

(setq default-line-spacing 3)

(show-paren-mode)
(setq show-paren-style 'expression)
