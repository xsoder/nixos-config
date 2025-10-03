;; -*- lexical-binding: t; -*-
(require 'subr-x)
(require 'mmm-mode)
(require 'simpc-mode)

;; Disable MMM-mode background highlighting globally
(setq mmm-submode-decoration-level 0)

;;; Define chc-html-mode derived from html-mode with no indentation
(define-derived-mode chc-html-mode html-mode "ChcHTML"
  "Custom HTML mode for CHC templates with embedded C blocks."
  ;; Disable automatic indentation
  (setq-local indent-line-function 'ignore)
  ;; Disable MMM highlighting for this mode
  (setq-local mmm-submode-decoration-level 0)
  ;; (Optional) Add or adjust font-lock rules for better tag detection
  ;; You can extend font-lock here if needed
  )

;;; Define mmm-mode class for C code inside @ ... @ delimiters
(mmm-add-classes
 '((chc-simpc-block
    :submode simpc-mode
    :face mmm-default-submode-face  ;; Use default (no highlighting)
    :front "^[ \t]*@\\s-*\n"  ;; start on a line with only @
    :back "^[ \t]*@\\s-*\n"   ;; end on a line with only @
    :include-front nil        ;; exclude @ delimiter lines from C mode
    :include-back nil
    :insert ((?@ simpc-block nil "@\n" . "\n@")))))

;; Ensure mmm-default-submode-face has no background
(set-face-attribute 'mmm-default-submode-face nil :background 'unspecified)

;;; Define chc-mode derived from chc-html-mode
(define-derived-mode chc-mode chc-html-mode "CHC"
  "Major mode for CHC template files mixing HTML and C via @ blocks."
  ;; Disable MMM decoration before enabling MMM mode
  (setq-local mmm-submode-decoration-level 0)
  (mmm-mode 1))

;;; Apply mmm class to chc-mode for files with .chc extension
(mmm-add-mode-ext-class 'chc-mode nil 'chc-simpc-block)

;;; Associate .chc files with chc-mode
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.chc\\'" . chc-mode))

(provide 'chc-mode)
