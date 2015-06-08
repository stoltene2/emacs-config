;; Put your theme loading and customization in here

(defun es/color-region-bg ()
  "Standard color used for bg-colors"
  "#303029")

(defun es/color-default-fg ()
  "Default forground color"
  "#F8F8F2")

(if (eq system-type 'darwin)
    (set-default-font "Monaco-16")
  (set-default-font "SourceCodePro-16"))

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



(set-face-attribute 'region nil :background (region-bg-color))


;; Ediff custom faces
(set-face-attribute 'ediff-even-diff-A nil :background (es/color-region-bg) :foreground (es/color-default-fg))
(set-face-attribute 'ediff-even-diff-B nil :background (es/color-region-bg) :foreground (es/color-default-fg))
(set-face-attribute 'ediff-even-diff-C nil :background (es/color-region-bg) :foreground (es/color-default-fg))
(set-face-attribute 'ediff-odd-diff-A nil :background (es/color-region-bg) :foreground (es/color-default-fg))
(set-face-attribute 'ediff-odd-diff-Ancestor nil :background (es/color-region-bg) :foreground "cyan3")
(set-face-attribute 'ediff-odd-diff-B nil :background (es/color-region-bg) :foreground (es/color-default-fg))
(set-face-attribute 'ediff-odd-diff-C nil :background (es/color-region-bg) :foreground (es/color-default-fg))
