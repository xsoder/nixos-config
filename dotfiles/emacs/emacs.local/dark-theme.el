;;; dark-theme.el --- Casey Muratori's Emacs Theme with custom minibuffer -*- lexical-binding: t; -*-]

(deftheme dark
  "A dark theme based on Casey Muratori's editor colors.")
(let ((class '((class color) (min-colors 89)))
      ;; Base colors from Casey's theme
      (bg        "#161616")
      (fg        "#ffffff")
      (cursor    "#40FF40")
      (hl-line   "#191970")
      (selection "#191970")
      (bracket   "#cdaa7d")
      (string    "#6b8e23")
      (comment   "#7f7f7f")
      (keyword   "#cd950c")
      (constant  "#6b8e23")
      (invalid   "#f44747")
      (command   "#61afef")
      (purple    "#654CA8"))
  (custom-theme-set-faces
   'dark
   `(default ((,class (:background ,bg :foreground ,fg))))
   `(cursor ((,class (:background ,cursor))))
   `(hl-line ((,class (:background ,hl-line))))
   `(region ((,class (:background ,selection))))
   `(highlight ((,class (:background ,hl-line))))
   `(fringe ((,class (:background ,bg :foreground ,fg))))
   `(show-paren-match ((,class (:background ,purple :foreground ,fg :weight bold))))
   `(show-paren-mismatch ((,class (:background ,invalid :foreground ,fg :weight bold))))
   
   ;; Search highlighting
   `(isearch ((,class (:background ,purple :foreground ,fg :weight bold))))
   `(lazy-highlight ((,class (:background ,purple :foreground ,fg :weight normal))))
   `(isearch-fail ((,class (:background ,invalid :foreground ,fg :weight bold))))
   
   ;; Syntax highlighting (no italics)
   `(font-lock-keyword-face       ((,class (:foreground ,keyword :weight bold :slant normal))))
   `(font-lock-builtin-face       ((,class (:foreground ,bracket :slant normal))))
   `(font-lock-type-face          ((,class (:foreground ,bracket :slant normal))))
   `(font-lock-variable-name-face ((,class (:foreground ,bracket :slant normal))))
   `(font-lock-function-name-face ((,class (:foreground ,bracket :slant normal))))
   `(font-lock-constant-face      ((,class (:foreground ,constant :slant normal))))
   `(font-lock-string-face        ((,class (:foreground ,string :slant normal))))
   `(font-lock-comment-face       ((,class (:foreground ,comment :slant normal))))
   `(font-lock-warning-face       ((,class (:foreground ,invalid :weight bold :slant normal))))
   ;; Minibuffer / completion tweaks
   `(minibuffer-prompt ((,class (:foreground ,fg :weight bold :slant normal))))
   `(completions-common-part ((,class (:foreground ,command :weight normal :slant normal)))) ;; commands in blue
   `(completions-first-difference ((,class (:foreground ,cursor :weight bold :slant normal))))
   `(completions-highlight ((,class (:background ,cursor :foreground ,bg :slant normal))))
   ;; Vertico/Selectrum/Ivy/icomplete
   `(vertico-current ((,class (:background ,cursor :foreground ,bg :slant normal))))
   `(ivy-current-match ((,class (:background ,cursor :foreground ,bg :slant normal))))
   `(icomplete-first-match ((,class (:foreground ,command :weight bold :slant normal))))
   ;; IDO & Smex
   `(ido-first-match ((,class (:foreground ,command :weight bold :slant normal))))
   `(ido-only-match  ((,class (:foreground ,command :weight bold :slant normal))))
   `(ido-subdir      ((,class (:foreground ,fg :weight normal :slant normal))))
   `(ido-indicator   ((,class (:foreground ,fg :weight normal :slant normal))))
   `(smex-item-selected ((,class (:background ,cursor :foreground ,bg :weight bold :slant normal))))
   `(smex-keyword ((,class (:foreground ,command :weight normal :slant normal))))
   ;; Explicitly style command names (works for M-x etc.)
   `(compilation-info ((,class (:foreground ,command :weight normal :slant normal))))
   `(help-key-binding ((,class (:foreground ,command :weight normal :slant normal))))
   ;; Dired (dirs in blue, rest white)
   `(dired-directory ((,class (:foreground ,command :weight bold :slant normal))))
   `(dired-symlink   ((,class (:foreground ,fg :weight normal :slant normal))))
   `(dired-marked    ((,class (:foreground ,fg :weight normal :slant normal))))
   `(dired-flagged   ((,class (:foreground ,invalid :weight bold :slant normal))))))
(provide-theme 'dark)
;;; dark-theme.el ends here
