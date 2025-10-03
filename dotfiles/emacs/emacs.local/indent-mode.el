;;; indent-mode.el  -*- lexical-binding: t; -*-
;; Spaces → '.', Tabs → '>'
;; Automatically enabled in programming/text modes

;;; Faces
(defface indent-mode-space-face
  '((t (:weight normal :inherit nil)))
  "Face for space dots (will use theme comment color dynamically).")

(defface indent-mode-tab-face
  '((t (:weight normal :inherit nil)))
  "Face for tab markers (will use theme comment color dynamically).")

;;; Minor mode
(define-minor-mode indent-mode
  "Show leading indentation with dots for spaces and '>' for tabs using font-lock."
  :lighter " IndentMarks"
  (if indent-mode
      (progn
        ;; Set the face colors dynamically from theme
        (set-face-foreground 'indent-mode-space-face (face-foreground 'font-lock-comment-face))
        (set-face-foreground 'indent-mode-tab-face   (face-foreground 'font-lock-comment-face))
        ;; Add font-lock rule
        (font-lock-add-keywords
         nil
         '(("^[ \t]+"
            (0 (let ((start (match-beginning 0))
                     (end (match-end 0)))
                 (put-text-property start end 'display
                                    (apply #'concat
                                           (mapcar (lambda (c)
                                                     (cond ((eq c ?\t) 
                                                            (propertize ">" 'face 'indent-mode-tab-face))
                                                           ((eq c ?\s)
                                                            (propertize "." 'face 'indent-mode-space-face))))
                                                   (string-to-list
                                                    (buffer-substring-no-properties start end)))))
                 nil)))))
        (font-lock-flush)
        (font-lock-ensure))
    ;; Disable font-lock
    (font-lock-remove-keywords
     nil
     '(("^[ \t]+"
        (0 (let ((start (match-beginning 0))
                 (end (match-end 0)))
             (put-text-property start end 'display nil)
             nil)))))
    (font-lock-flush)
    (font-lock-ensure)))

;;; Automatically enable in programming/text modes
(dolist (hook '(prog-mode-hook text-mode-hook))
  (add-hook hook (lambda () (indent-mode 1))))

(provide 'indent-mode)
