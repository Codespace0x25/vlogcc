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
    ;; Braces and parens
    (modify-syntax-entry ?{ "(}" st)
    (modify-syntax-entry ?} "){" st)
    (modify-syntax-entry ?\( "()" st)
    (modify-syntax-entry ?\) ")(" st)
    st)
  "Syntax table for `vlog-mode'.")



(defconst vlog-font-lock-keywords
  (let* (
         ;; Keywords
         (keywords '("defun" "extern" "return" "require" "if" "else"
                     "const" "struct" "typedef" "mut" "type" "macro" "#include"
                     "while" "for" "break" "continue"))
         ;; Types
         (types '("int" "float" "double" "char" "void" "String" "bool"
                  "vector" "hashmap"))

         (keyword-regexp (regexp-opt keywords 'words))
         (type-regexp (regexp-opt types 'words)))

    `(
      (,keyword-regexp . font-lock-keyword-face)
      (,type-regexp . font-lock-type-face)
      ("\"[^\"]*\"" . font-lock-string-face)       ;; Strings
      ("\\<\\([0-9]+\\)\\>" . font-lock-constant-face) ;; Numbers
      ("\\<\\(true\\|false\\)\\>" . font-lock-constant-face) ;; Booleans
      ("\\<\\([A-Za-z_][A-Za-z0-9_]*\\)\\s-*(" 1 font-lock-function-name-face) ;; Function calls
      ("^\\s-*#\\w+" . font-lock-preprocessor-face) ;; Preprocessor
      ("\\<macro\\s-+\\([A-Za-z_][A-Za-z0-9_]*\\)" 1 font-lock-constant-face) ;; Macros
      )))

(defun vlog-indent-line ()
  "Indent current line as vlog code."
  (interactive)
  (let ((indent-level 0)
        (not-indented t)
        (cur-indent 0))
    (save-excursion
      (beginning-of-line)
      (if (bobp)
          (setq not-indented nil)
        (if (looking-at "^[ \t]*}")
            (progn
              (save-excursion
                (forward-line -1)
                (setq cur-indent (- (current-indentation) tab-width)))
              (setq not-indented nil))
          (while not-indented
            (forward-line -1)
            (if (looking-at "^[ \t]*}")
                (setq indent-level (current-indentation))
              (if (looking-at ".*{[ \t]*$")
                  (setq indent-level (+ (current-indentation) tab-width))
                (setq indent-level (current-indentation))))
            (setq not-indented nil)))))
    (if (> indent-level 0)
        (indent-line-to indent-level)
      (indent-line-to 0))))

;;;###autoload
(define-derived-mode vlog-mode prog-mode "vlog"
  "Major mode for editing vlog code."
  :syntax-table vlog-mode-syntax-table
  (setq font-lock-defaults '((vlog-font-lock-keywords)))
  (setq-local indent-line-function 'vlog-indent-line)
  (setq-local standard-indent 2)
  (electric-indent-local-mode 1)
  (show-paren-mode 1))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.code\\'" . vlog-mode))

(provide 'vlog-mode)
;;; vlog-mode.el ends here
