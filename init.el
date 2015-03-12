;; Allow this to function outside of an emacs.d directory
(setq debug-on-error t)

(if (not (boundp 'user-emacs-directory))
    (setq user-emacs-directory "~/.emacs.d/"))

(setenv "PATH" (concat "/usr/local/bin:"
                       (getenv "PATH")))

(setq exec-path (append exec-path '("/usr/local/bin")))


(setq el-get-dir
      (let* ((current-dir-name
              (file-name-directory (or load-file-name (buffer-file-name)))))
        (concat current-dir-name "el-get")))


(add-to-list 'load-path (concat (file-name-as-directory el-get-dir) "el-get"))

;;(package-initialize)

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Load package inits and sync packages
(setq el-get-user-package-directory "~/.emacs.d/packages")

(if (file-exists-p el-get-user-package-directory)
    (let*
        ((files (directory-files el-get-user-package-directory nil "^init-.*\.el$"))

         (remove-init-ext (lambda (f)
                             (file-name-sans-extension
                              (mapconcat 'identity (cdr (split-string f "-")) "-"))))

         (packages (mapcar remove-init-ext files)))

      (setq my-packages packages))
  (message "There are no packages found in %s" el-get-user-package-directory))

(el-get 'sync my-packages)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load custom initialization for after packages have loaded

(message "My emacs directory is: %s" user-emacs-directory)

(mapcar (lambda (f)
          (message "loading %s" f)
          (load-file f))
        (file-expand-wildcards (concat user-emacs-directory "init/*.el")))



(setq debug-on-error nil)
