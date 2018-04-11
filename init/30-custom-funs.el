(defadvice async-shell-command (before buffer-named-with-command
                                       (command &optional output-buffer error-buffer))
  (when (null output-buffer)
    (setq output-buffer (switch-to-buffer (concat "*Async: " command "*")))))
(ad-activate 'async-shell-command)

(defadvice shell-command (before buffer-named-with-command
                                 (command &optional output-buffer error-buffer))
  (when (null output-buffer)
    (setq output-buffer (switch-to-buffer (concat "*Shell: " command "*")))))
(ad-activate 'shell-command)


(defun es/toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(defun es/rotate-windows ()
  "Rotate your windows"
  (interactive)
  (let* ((i 0)
         (numWindows 0))
    (cond ((not (> (count-windows) 1))
           (message "You can't rotate a single window!"))
          (t
           (setq i 1)
           (setq numWindows (count-windows))
           (while  (< i numWindows)
             (let* (
                    (w1 (elt (window-list) i))
                    (w2 (elt (window-list) (+ (% i numWindows) 1)))

                    (b1 (window-buffer w1))
                    (b2 (window-buffer w2))

                    (s1 (window-start w1))
                    (s2 (window-start w2))
                    )
               (set-window-buffer w1  b2)
               (set-window-buffer w2 b1)
               (set-window-start w1 s2)
               (set-window-start w2 s1)
               (setq i (1+ i))))))))

(defun es/open-line-below ()
  (interactive)
  (end-of-line)
  (newline-and-indent)
  (indent-for-tab-command))

(defun es/open-line-above ()
  (interactive)
  (beginning-of-line)
  (newline-and-indent)
  (forward-line -1)
  (indent-for-tab-command))


;; Re-indent pastes
;; This came from the emacs wiki
;; http://emacswiki.org/emacs/AutoIndentation
(dolist (command '(yank yank-pop))
   (eval `(defadvice ,command (after indent-region activate)
            (and (not current-prefix-arg)
                 (member major-mode '(emacs-lisp-mode js2-mode web-mode typescript-mode))
                 (let ((mark-even-if-inactive transient-mark-mode))
                   (indent-region (region-beginning) (region-end) nil))))))

;; Remove indent when kill line at end of line
(defadvice kill-line (before check-position activate)
  (if (member major-mode
              '(emacs-lisp-mode js2-mode web-mode))
      (if (and (eolp) (not (bolp)))
          (progn (forward-char 1)
                 (just-one-space 0)
                 (backward-char 1)))))

;; This should be removable now
(defun es/grab-constructor-name ()
  "Grab the name of the constructor being used in js class.

This above the current snippet expansion to find the name of the constructor used before the first use of .prototype."
  (save-excursion
    (save-match-data
      (save-restriction
        (progn
          (widen)
          (goto-char (point-min))
          (if (re-search-forward "\\b\\(.*?\\)\\.prototype\\." nil t)
              (match-string-no-properties 1)
            "Class"))))))


(defun es/find-class-from-module-string (str)
  "Given a dot separated module string this yields the last
  component"
  (car (last (s-split "\\." str))))

;;;
(defun es/find-template-other-window ()
  "See if there is a directive template and jump there"
  (interactive)

  (let* ((directive-template-path (es/guess-template-file)))
    (if (and directive-template-path (file-readable-p directive-template-path))
        (find-file-other-window directive-template-path)
      (message (format "Could not find template file %s" directive-template-path)))))

(defun es/guess-template-file ()
  "Guesses the template file for an angular directive"
  (save-excursion
    (save-match-data
      (beginning-of-buffer)
      (let* ((app-root-dir (if (boundp 'es/angular-project-root)
                               es/angular-project-root
                             ""))

             (found-template-p (re-search-forward "^\s*templateUrl\s*:\s*'\\(.*?\.html\\)'\s*,?\s*$" nil t)))
        (if found-template-p
            (let* ((matched-text (match-string 1))
                   (is-relative-path (s-prefix-p "." matched-text)))
              (if is-relative-path
                  matched-text
                (concat app-root-dir (match-string 1)))))))))


(defmacro es/search-and-collapse (search-cmd str-or-regex)
  "Search using the provided function and string

search-cmd is typically 're-search-forward or
'search-forward. str-or-regexp is self explanatory"
  `(save-excursion
    (save-match-data
      (beginning-of-buffer)
      (while (,search-cmd ,str-or-regex nil t)
        (end-of-line)
        (js2-mode-hide-element)))))

(defun es/collapse-all-functions ()
  "Collapse all named functions and prototype functions"
  (interactive)
  ;; Angular specific patterns
  (es/search-and-collapse re-search-forward "^\s*vm\..*function")

  ;; Jasmine related functions
  (es/search-and-collapse search-forward "it(")
  (es/search-and-collapse re-search-forward "beforeEach.*function")

  ;; Straight JS functions
  (es/search-and-collapse re-search-forward "^\s*function\s")
  (es/search-and-collapse re-search-forward "^\s*this\..*function")
  (es/search-and-collapse search-forward ".prototype."))


;;; Merge ediff region A and B into C
(defun es/ediff-copy-both-to-C ()
  (interactive)
  (ediff-copy-diff ediff-current-difference nil 'C nil
                   (concat
                    (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                    (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))

(defun es/add-d-to-ediff-mode-map () (define-key ediff-mode-map "d" 'es/ediff-copy-both-to-C))
(add-hook 'ediff-keymap-setup-hook 'es/add-d-to-ediff-mode-map)


;;; collapse multiple blank lines down to one
(defun es/remove-multiple-emtpy-lines ()
  "Removes multiple empty lines from a file"
  (interactive)
  (let* ((blank-line-re "^\n\\{2,\\}")
         (replacement "\n"))
    (save-excursion (progn
                      (goto-char (point-min))
                      (while (re-search-forward blank-line-re nil t)
                        (replace-match replacement nil nil))))))


(defun es/file-exists-at-point ()
  "Find if the path under the cursor exists.

This reports to the message buffer if we can find the file or
not."
  (interactive)
  (if (file-exists-p (ffap-string-at-point))
      (message "File exists")
    (message "Cannot find file")))


(defvar es/git-server
  "http://remote.repo.com/path#"
  "Used for replacing contents in NPM for testing")

(defun es/replace-branch-name-selection-with-git-branch ()
  "This will generate the NPM location from the branch provided from es/git-server
  string at point. To use, highlight region and it will be prefixed by a git path"
  (interactive)
  (if (use-region-p)

      (let*
          ((selected-region (delete-and-extract-region (region-beginning) (region-end))))
        (insert (concat es/git-server selected-region)))

    (message "You must have an active region to replace")))


(defun es/use-tslint-from-node-modules ()
  "Load tslint from local node_modules if available.
Given to me by Surya."
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (tslint (and root
                      (expand-file-name "node_modules/.bin/tslint" root))))

    (when (and tslint (file-executable-p tslint))
      (setq-local flycheck-typescript-tslint-executable tslint))))



(defun es/typescript-helm-projectile-insert-file-at-point ()
  "Insert a file at point from your git tree"
  (interactive)
  (let* ((project-root (projectile-project-root))
         (project-files (projectile-current-project-files))
         (files (projectile-select-files project-files)))
    (if (= (length files) 1)
        (insert (expand-file-name (car files) (projectile-project-root)))
      (helm :sources (helm-build-sync-source "Projectile files"
                       :candidates (if (> (length files) 1)
                                       (helm-projectile--files-display-real files project-root)
                                     (helm-projectile--files-display-real project-files project-root))
                       :fuzzy-match helm-projectile-fuzzy-match
                       :action-transformer 'helm-find-files-action-transformer
                       :keymap helm-projectile-find-file-map
                       :help-message helm-ff-help-message
                       :mode-line helm-read-file-name-mode-line-string
                       :action (lambda (filename)
                                 (let* ((relative-file (file-relative-name filename default-directory))
                                        (trimmed-file (s-replace-all '((".d.ts" . "") (".ts" . "") (".css" . "") (".js" . "")) relative-file)))
                                   (insert trimmed-file)))
                       :persistent-action #'helm-projectile-file-persistent-action
                       :persistent-help "Preview file")
            :buffer "*helm projectile*"
            :truncate-lines helm-projectile-truncate-lines
            :prompt (projectile-prepend-project-name "Find file: ")))))
