;; Allow this to function outside of an emacs.d directory
(setq debug-on-error t)

(if (not (boundp 'user-emacs-directory))
    (setq user-emacs-directory "~/.emacs.d/"))

(setenv "PATH" (concat "/usr/local/bin:"
                       (getenv "PATH")))

(setq exec-path (cons "/usr/local/bin" exec-path ))

(setq el-get-dir
      (let* ((current-dir-name
              (file-name-directory (or load-file-name (buffer-file-name)))))
        (concat current-dir-name "el-get")))


(add-to-list 'load-path (concat (file-name-as-directory el-get-dir) "el-get"))


(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))



;; Have el-get sync with elpa and melpa
;; Periodically refresh with (el-get-elpa-build-local-recipes)
;; This caches the recipies

(require 'el-get-elpa)
(unless (file-directory-p el-get-recipe-path-elpa)
  (el-get-elpa-build-local-recipes))

(require 'package)
(add-to-list 'package-archives
             ;; The 't' means to append, so that MELPA comes after the more
             ;; stable ELPA archive.
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Switch to use-package

;;(package-initialize)

;; (unless (package-installed-p 'use-package)
;;   (package-refresh-contents)
;;   (package-install 'use-package))

;; ;; Installed via el-get now
;; (eval-when-compile (require 'use-package))
;; (use-package jasminejs-mode
;;   :ensure t
;;   :bind (:map jasminejs-prefix-map) "C-c j" )


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
(put 'narrow-to-region 'disabled nil)
