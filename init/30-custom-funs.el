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
  (cond ((not (> (count-windows)1))
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
             (setq i (1+ i)))))))

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


(setq es/font-sizes '( "12" "14" "16" "20" "24"))

(defun es/cycle-font-size ()
  "Cycle between 14, 16, 18, 24 pt fonts"
  (interactive)
  (let* ((size (car es/font-sizes))
         (font (concat "Monaco-" size)))
    (progn (setq es/font-sizes (-rotate -1 es/font-sizes))
           (set-default-font font))))


;; Re-indent pasts
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
