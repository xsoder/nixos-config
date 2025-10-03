;;; vlog-mode.el --- Major mode for the vlog language -*- lexical-binding: nil -*-

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
	 (keywords '("defun" "extern" "return" "require" "if" "else"
		     "const" "struct" "typedef" "mut" "type" "macro" "#include"
		     "while" "if" "else"))
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
      ("\\<macro\\s-+\\([A-Za-z_][A-Za-z0-9_]*\\)" 1 font-lock-constant-face) ;; Macros
      ("\\<\\([A-Za-z_][A-Za-z0-9_]*\\)\\s-*(" 1 font-lock-function-name-face) ;; Function calls
      )))

;;;###autoload
(define-derived-mode vlog-mode prog-mode "vlog"
  "Major mode for editing vlog code."
  :syntax-table vlog-mode-syntax-table
  (setq font-lock-defaults '((vlog-font-lock-keywords)))
  (setq tab-width 4)
  (setq indent-tabs-mode nil)
  (setq-local standard-indent 4)
  (setq-local c-basic-offset 4)
  (setq-local tab-width 4))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.code\\'" . vlog-mode))

(provide 'vlog-mode)
;;; vlog-mode.el ends here
