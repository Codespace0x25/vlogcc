;;; vlog-mode.el --- Major mode for the vlog language

(defvar vlog-mode-hook nil)

(defvar vlog-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; Treat underscore as part of words
    (modify-syntax-entry ?_ "w" st)
    ;; C-style comments
    (modify-syntax-entry ?/ ". 124b" st)
    (modify-syntax-entry ?* ". 23" st)
    (modify-syntax-entry ?\n "> b" st)
    st)
  "Syntax table for `vlog-mode'.")

(defconst vlog-font-lock-keywords
  (let* (
         ;; Keywords
         (keywords '("defun" "extern" "return" "require" "if" "else" "const" "struct" "typedef" "mut"))
         ;; Types
         (types '("int" "float" "double" "char" "void" "string" "bool"))

         (keyword-regexp (regexp-opt keywords 'words))
         (type-regexp (regexp-opt types 'words)))

    `(
      (,keyword-regexp . font-lock-keyword-face)
      (,type-regexp . font-lock-type-face)
      ("\"[^\"]*\"" . font-lock-string-face)       ;; Strings
      ("\\<\\([0-9]+\\)\\>" . font-lock-constant-face) ;; Numbers
      ("\\<\\(true\\|false\\)\\>" . font-lock-constant-face) ;; Booleans
      ("\\<\\([A-Za-z_][A-Za-z0-9_]*\\)\\s-*(" 1 font-lock-function-name-face) ;; Function calls
      )))

;;;###autoload
(define-derived-mode vlog-mode prog-mode "vlog"
  "Major mode for editing vlog code."
  :syntax-table vlog-mode-syntax-table
  (setq font-lock-defaults '((vlog-font-lock-keywords)))
  (setq tab-width 2)
  (setq indent-tabs-mode nil)
  (setq-local standard-indent 2)
  (setq-local c-basic-offset 2)
  (setq-local tab-width 2))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.code\\'" . vlog-mode))

(provide 'vlog-mode)
;;; vlog-mode.el ends here
