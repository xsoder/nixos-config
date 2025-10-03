;;; vterm-buffer.el --- Use dmenu to switch between open vterm buffers -*- lexical-binding: t; -*-

(require 'vterm)

(defvar vterm-buffer-dmenu-command "dmenu -p \"Select vterm:\" -c -i -l 20"
  "Command to invoke dmenu with options.
You can customize this string to add flags or use rofi.")

(defun vterm-buffer-list ()
  "Return a list of all open vterm buffers."
  (seq-filter
   (lambda (buf)
     (with-current-buffer buf
       (eq major-mode 'vterm-mode)))
   (buffer-list)))

;;;###autoload
(defun vterm-switch-buffer-dmenu ()
  "Use dmenu to select and switch to an open vterm buffer."
  (interactive)
  (let* ((buffers (vterm-buffer-list))
         (names (mapcar #'buffer-name buffers))
         (input (string-join names "\n"))
         (choice (with-temp-buffer
                   (insert input)
                   ;; Call dmenu and get selected buffer name
                   (let ((exit-code (call-process-region
                                     (point-min) (point-max)
                                     shell-file-name
                                     t
                                     t
                                     nil
                                     shell-command-switch
                                     vterm-buffer-dmenu-command)))
                     (if (eq exit-code 0)
                         (string-trim (buffer-string))
                       nil)))))
    (if (and choice (member choice names))
        (switch-to-buffer choice)
      (message "No vterm buffer selected or buffer does not exist"))))

(provide 'vterm-buffer)
;;; vterm-buffer.el ends here
