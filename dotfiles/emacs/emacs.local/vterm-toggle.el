;;; vterm-toggle.el --- Toggle vterm buffer with one key -*- lexical-binding: t; -*-

(require 'vterm)
(require 'cl-lib)

(defun vterm-toggle--close-all-vterm-windows ()
  "Close all visible vterm windows and bury their buffers."
  (let ((windows-to-delete '())
        (current-window (selected-window)))
    ;; First pass: identify vterm windows and bury their buffers
    (dolist (window (window-list))
      (let ((buffer (window-buffer window)))
        (when (and buffer 
                   (string-match-p "\\*vterm:" (buffer-name buffer))
                   (with-current-buffer buffer (derived-mode-p 'vterm-mode)))
          (bury-buffer buffer)
          (when (and (window-live-p window)
                     (> (length (window-list)) 1))
            (push window windows-to-delete)))))
    
    ;; Second pass: delete the windows, handling the selected window last
    (let ((selected-vterm-window nil))
      (dolist (window windows-to-delete)
        (if (eq window current-window)
            (setq selected-vterm-window window)
          (when (window-live-p window)
            (delete-window window))))
      
      ;; Delete the selected vterm window last if it exists
      (when (and selected-vterm-window 
                 (window-live-p selected-vterm-window)
                 (> (length (window-list)) 1))
        (delete-window selected-vterm-window)))))

(defun vterm-toggle--any-vterm-visible-p ()
  "Return t if any vterm window is currently visible."
  (cl-some
   (lambda (window)
     (let ((buffer (window-buffer window)))
       (and buffer
            (string-match-p "\\*vterm:" (buffer-name buffer))
            (with-current-buffer buffer (derived-mode-p 'vterm-mode)))))
   (window-list)))

;;;###autoload
(defun vterm-toggle-split ()
  "Toggle vterm buffer in horizontal split for current buffer's directory.
If any vterm is visible, close all vterm windows.
If none are visible, open vterm in the current buffer's directory."
  (interactive)
  (let* ((default-dir (or (and (buffer-file-name) 
                               (file-name-directory (buffer-file-name)))
                          default-directory))
         (vterm-buf-name (format "*vterm: %s*" (abbreviate-file-name default-dir)))
         (vterm-buf (get-buffer vterm-buf-name)))
    
    (if (vterm-toggle--any-vterm-visible-p)
        (progn
          (vterm-toggle--close-all-vterm-windows)
          (message "Closed existing vterm windows"))
      ;; Open vterm in horizontal split
      (let ((new-win (split-window-below))) ; Changed to vertical split (new window below)
        (select-window new-win)
        (if (and vterm-buf (buffer-live-p vterm-buf))
            (progn
              (switch-to-buffer vterm-buf)
              ;; Change to the correct directory if needed
              (when (not (string= default-directory default-dir))
                (vterm-send-string (format "cd %s" (shell-quote-argument default-dir)))
                (vterm-send-return)))
          (let ((default-directory default-dir))
            (vterm vterm-buf-name)))))))

;;;###autoload
(defun vterm-toggle-fullscreen ()
  "Toggle fullscreen vterm buffer.
If any vterm is visible, close all and exit.
If none visible, open vterm fullscreen in the current buffer's directory."
  (interactive)
  (if (vterm-toggle--any-vterm-visible-p)
      (progn
        (vterm-toggle--close-all-vterm-windows)
        (message "Closed existing vterm windows"))
    ;; Open fullscreen vterm
    (let* ((default-dir (or (and (buffer-file-name)
                                 (file-name-directory (buffer-file-name)))
                            default-directory))
           (vterm-buf-name (format "*vterm: %s*" (abbreviate-file-name default-dir)))
           (vterm-buf (get-buffer vterm-buf-name)))
      
      (delete-other-windows)
      (if (and vterm-buf (buffer-live-p vterm-buf))
          (progn
            (switch-to-buffer vterm-buf)
            ;; Change to the correct directory if needed
            (when (not (string= default-directory default-dir))
              (vterm-send-string (format "cd %s" (shell-quote-argument default-dir)))
              (vterm-send-return)))
        (let ((default-directory default-dir))
          (vterm vterm-buf-name))))))

;;;###autoload  
(defun vterm-toggle-new-window ()
  "Toggle vterm buffer in a new window (frame).
If any vterm is visible, close all vterm windows.
If none are visible, open vterm in a new window/frame."
  (interactive)
  (if (vterm-toggle--any-vterm-visible-p)
      (progn
        (vterm-toggle--close-all-vterm-windows)
        (message "Closed existing vterm windows"))
    ;; Open vterm in new frame
    (let* ((default-dir (or (and (buffer-file-name)
                                 (file-name-directory (buffer-file-name)))
                            default-directory))
           (vterm-buf-name (format "*vterm: %s*" (abbreviate-file-name default-dir)))
           (vterm-buf (get-buffer vterm-buf-name))
           (new-frame (make-frame)))
      
      (select-frame new-frame)
      (if (and vterm-buf (buffer-live-p vterm-buf))
          (progn
            (switch-to-buffer vterm-buf)
            ;; Change to the correct directory if needed
            (when (not (string= default-directory default-dir))
              (vterm-send-string (format "cd %s" (shell-quote-argument default-dir)))
              (vterm-send-return)))
        (let ((default-directory default-dir))
          (vterm vterm-buf-name))))))

;;;###autoload
(defun vterm-toggle-vertical-split ()
  "Toggle vterm buffer in vertical split (side panel) for current buffer's directory.
If any vterm is visible, close all vterm windows.
If none are visible, open vterm in a vertical split to the side."
  (interactive)
  (let* ((default-dir (or (and (buffer-file-name) 
                               (file-name-directory (buffer-file-name)))
                          default-directory))
         (vterm-buf-name (format "*vterm: %s*" (abbreviate-file-name default-dir)))
         (vterm-buf (get-buffer vterm-buf-name)))
    
    (if (vterm-toggle--any-vterm-visible-p)
        (progn
          (vterm-toggle--close-all-vterm-windows)
          (message "Closed existing vterm windows"))
      ;; Open vterm in vertical split (side panel)
      (let ((new-win (split-window-right))) ; Create vertical split (new window to the right)
        (select-window new-win)
        (if (and vterm-buf (buffer-live-p vterm-buf))
            (progn
              (switch-to-buffer vterm-buf)
              ;; Change to the correct directory if needed
              (when (not (string= default-directory default-dir))
                (vterm-send-string (format "cd %s" (shell-quote-argument default-dir)))
                (vterm-send-return)))
          (let ((default-directory default-dir))
            (vterm vterm-buf-name)))))))



(provide 'vterm-toggle)
;;; vterm-toggle.el ends here
