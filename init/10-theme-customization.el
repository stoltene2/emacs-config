;; Put your theme loading and customization in here

(if (fboundp 'set-frame-font)
    (if (eq system-type 'darwin)
	(set-frame-font "Monaco-16")
      (set-frame-font "SourceCodePro-16")))
