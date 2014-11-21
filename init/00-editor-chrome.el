;; Place custom editor visualizations in here.
;; Example, turn on/off menu bars, toolbars, scrollbars.

(dolist (mode '(menu-bar-mode tool-bar-mode (scroll-bar-mode))
              (when (fboundp mode)
                (funcall mode -1))))

;; No splash screen please ... jeez
(setq inhibit-startup-message t)
