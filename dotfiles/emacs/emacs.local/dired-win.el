;;; dired-win.el --- auto-copy and move to other dired window -*- lexical-binding: t; -*-

(defun my/dired-get-other-dired-window ()
  "Return the other visible window with a Dired buffer, or nil if none."
  (get-window-with-predicate
   (lambda (win)
     (with-current-buffer (window-buffer win)
       (and (not (eq win (selected-window)))
            (eq major-mode 'dired-mode))))))

(defun my/dired-copy-to-other-dired-window ()
  "Copy marked files/directories to the other Dired window's directory if available.
Fallback to normal `dired-do-copy' prompting otherwise."
  (interactive)
  (let* ((marked-files (dired-get-marked-files nil nil))
         (other-win (my/dired-get-other-dired-window))
         (target-dir (when other-win
                       (with-current-buffer (window-buffer other-win)
                         (dired-current-directory)))))
    (if (and target-dir marked-files)
        (progn
          (dolist (file marked-files)
            (let ((target (expand-file-name (file-name-nondirectory file) target-dir)))
              (if (file-directory-p file)
                  (copy-directory file target t t)
                (copy-file file target 1))))
          ;; Refresh both Dired buffers:
          (revert-buffer)
          (with-current-buffer (window-buffer other-win)
            (revert-buffer))
          (message "Copied %d item(s) to %s" (length marked-files) target-dir))
      (call-interactively #'dired-do-copy))))

(defun my/dired-move-to-other-dired-window ()
  "Move marked files/directories to the other Dired window's directory if available.
Fallback to normal `dired-do-rename' prompting otherwise."
  (interactive)
  (let* ((marked-files (dired-get-marked-files nil nil))
         (other-win (my/dired-get-other-dired-window))
         (target-dir (when other-win
                       (with-current-buffer (window-buffer other-win)
                         (dired-current-directory)))))
    (if (and target-dir marked-files)
        (progn
          (dolist (file marked-files)
            (let ((target (expand-file-name (file-name-nondirectory file) target-dir)))
              (rename-file file target 1)))
          ;; Refresh both Dired buffers:
          (revert-buffer)
          (with-current-buffer (window-buffer other-win)
            (revert-buffer))
          (message "Moved %d item(s) to %s" (length marked-files) target-dir))
      (call-interactively #'dired-do-rename))))

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "C") #'my/dired-copy-to-other-dired-window)
  (define-key dired-mode-map (kbd "R") #'my/dired-move-to-other-dired-window))

(provide 'dired-win)
;;; dired-win.el ends here
