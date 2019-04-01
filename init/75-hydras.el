;;; 75-hydras.el --- Custome hydras for major-hydra-mode  -*- lexical-binding: t; -*-

;; Typescript

(major-mode-hydra-bind typescript-mode "Inspect"
  ("ir" tide-references "references"))

(major-mode-hydra-bind typescript-mode "Errors"
  ("el" flycheck-list-errors "list")
  ("ef" tide-fix "fix"))

(major-mode-hydra-bind typescript-mode "Refactor"
  ("r'" typescript-convert-to-template "str->template")
  ("rr" tide-rename-symbol "rename symbol"))

(major-mode-hydra-bind typescript-mode "File"
  ("ff" tide-format "format")
  ("fr" tide-rename-file "rename"))

(major-mode-hydra-bind typescript-mode "Server"
  ("st" tide-mode "enable")
  ("sc" tide-restart-server "connect/restart"))
