;;; url-grabber.el --- Grab URLs from buffer via dmenu -*- lexical-binding: t; -*-

(defvar url-grabber-dmenu-command "dmenu -p \"Select URL:\" -c -i -l 15"
  "Command to invoke dmenu for URL selection.")

(defun url-grabber--extract-urls ()
  "Extract all unique URLs from the current buffer."
  (let (urls)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              "\\(https?://[^\s\"'<>]+\\)" nil t)
        (push (match-string-no-properties 1) urls)))
    (delete-dups (nreverse urls))))

(defun url-grabber--select-url (urls)
  "Display URLS in dmenu and return the selected URL."
  (let ((input (string-join urls "\n"))
        (output-buffer (generate-new-buffer "*url-grabber-output*")))
    (with-temp-buffer
      (insert input)
      (let ((exit-code
             (call-process-region
              (point-min) (point-max)
              "sh" nil output-buffer nil "-c" url-grabber-dmenu-command)))
        (with-current-buffer output-buffer
          (let ((result (string-trim (buffer-string))))
            (kill-buffer output-buffer)
            (if (and (eq exit-code 0)
                     (string-match-p "^https?://" result))
                result
              nil))))))
  )

;;;###autoload
(defun url-grabber-copy ()
  "Grab URLs from buffer via dmenu and copy the selected one to the clipboard."
  (interactive)
  (let* ((urls (url-grabber--extract-urls))
         (selected (url-grabber--select-url urls)))
    (if selected
        (progn
          (kill-new selected)
          (when (fboundp 'gui-set-selection)
            (gui-set-selection 'CLIPBOARD selected))
          (message "Copied URL to clipboard: %s" selected))
      (message "No valid URL selected."))))

;;;###autoload
(defun url-grabber-open ()
  "Grab URLs from buffer via dmenu and open the selected one in the default browser."
  (interactive)
  (let* ((urls (url-grabber--extract-urls))
         (selected (url-grabber--select-url urls)))
    (if selected
        (progn
          (browse-url selected)
          (message "Opened URL: %s" selected))
      (message "No valid URL selected."))))

(provide 'url-grabber)
;;; url-grabber.el ends here
 
