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


(defvar es/font-sizes '( "10" "12" "13" "14" "15" "16" "17" "18" "24")
  "Font sizes used to cycle through while increasing the size")

(defun es/cycle-font-size ()
  "Cycle between 14, 16, 18, 24 pt fonts"
  (interactive)
  (let* ((size (car es/font-sizes))
         (font (concat "Monaco-" size)))
    (progn (setq es/font-sizes (-rotate -1 es/font-sizes))
           (message (format "Setting font to: %s" size))
           (set-default-font font))))

;; Re-indent pastes
;; This came from the emacs wiki
;; http://emacswiki.org/emacs/AutoIndentation
(dolist (command '(yank yank-pop))
   (eval `(defadvice ,command (after indent-region activate)
            (and (not current-prefix-arg)
                 (member major-mode '(emacs-lisp-mode js2-mode web-mode))
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
            (concat app-root-dir (match-string 1)))))))


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
