;; Put your theme loading and customization in here


(let ((set-font (if (fboundp 'set-default-font)
                    #'set-default-font
                  #'set-frame-font))
      (os-font (if (eq system-type 'darwin)
                   "Monaco-16"
                 "SourceCodePro-16")))
  (apply set-font '(os-font)))
