;;; colored-man.el --- show colored man pages -*- lexical-binding: t; -*-

(require 'man)
(require 'ansi-color)

;; Ensure man runs without a pager and emits color escapes (use less -R if needed)
;; If your Emacs doesn't inherit MANPAGER, set it here:
;; (setenv "MANPAGER" "less -R -X")
;; (setenv "MANROFFOPT" "-c")

(defun colored-man--apply-ansi ()
  "Convert ANSI escape sequences in the current buffer to faces."
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))

(defun colored-man--remap-faces ()
  "Make Man faces inherit the default face so theme colors apply."
  (dolist (f '(Man-overstrike Man-underline Man-reverse Man-italic Man))
    (when (facep f)
      (face-remap-add-relative f 'default))))

(defun colored-man--postprocess ()
  "Apply ansi colors and remap faces in Man buffers."
  (when (derived-mode-p 'Man-mode)
    (colored-man--apply-ansi)
    (font-lock-ensure)
    (colored-man--remap-faces)
    (setq-local buffer-read-only t)
    (visual-line-mode 1)
    (when (bound-and-true-p display-line-numbers-mode)
      (display-line-numbers-mode -1))))

(add-hook 'Man-mode-hook #'colored-man--postprocess)

;; Optional convenience wrapper to open man fullscreen
(defun colored-man (topic)
  "Open TOPIC man page fullscreen with colors."
  (interactive (list (read-string "Man: ")))
  (let ((cfg (current-window-configuration)))
    (man topic)
    (when-let ((b (get-buffer (format "*Man %s*" topic))))
      (switch-to-buffer b)
      (delete-other-windows)
      (add-hook 'kill-buffer-hook (lambda () (set-window-configuration cfg)) nil t))))

(provide 'colored-man)
;;; colored-man.el ends here

