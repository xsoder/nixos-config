;;; ssh-connect.el --- SSH connection helper for Emacs  -*- lexical-binding: t; -*-

;;; Commentary:
;; This script provides functions to connect to remote hosts via SSH
;; with prompts for hostname, username, and password.

;;; Code:

(defun ssh-connect ()
  "Connect to a remote host via SSH with prompts for credentials."
  (interactive)
  (let* ((hostname (read-string "Hostname/IP: "))
         (username (read-string "Username: "))
         (port (read-string "Port (default 22): " nil nil "22"))
         (remote-path (format "/ssh:%s@%s#%s:/" username hostname port)))
    
    ;; Validate inputs
    (when (or (string-empty-p hostname) (string-empty-p username))
      (error "Hostname and username are required"))
    
    (message "Connecting to %s@%s:%s..." username hostname port)
    
    ;; Open dired on remote host (will prompt for password)
    (dired remote-path)))

(defun ssh-connect-with-shell ()
  "Connect to a remote host via SSH and open a shell."
  (interactive)
  (let* ((hostname (read-string "Hostname/IP: "))
         (username (read-string "Username: "))
         (port (read-string "Port (default 22): " nil nil "22"))
         (buffer-name (format "*ssh-%s@%s*" username hostname)))
    
    ;; Validate inputs
    (when (or (string-empty-p hostname) (string-empty-p username))
      (error "Hostname and username are required"))
    
    (message "Opening SSH shell to %s@%s:%s..." username hostname port)
    
    ;; Create SSH shell using shell command
    (let ((default-directory (format "/ssh:%s@%s#%s:/" username hostname port)))
      (shell buffer-name))))

(defun ssh-connect-terminal ()
  "Connect to a remote host via SSH using term mode."
  (interactive)
  (let* ((hostname (read-string "Hostname/IP: "))
         (username (read-string "Username: "))
         (port (read-string "Port (default 22): " nil nil "22"))
         (buffer-name (format "*ssh-term-%s@%s*" username hostname)))
    
    ;; Validate inputs
    (when (or (string-empty-p hostname) (string-empty-p username))
      (error "Hostname and username are required"))
    
    (message "Opening SSH terminal to %s@%s:%s..." username hostname port)
    
    ;; Create terminal with SSH command
    (let ((ssh-command (format "ssh -p %s %s@%s" port username hostname)))
      (term ssh-command)
      (rename-buffer buffer-name t))))

(defun ssh-edit-file ()
  "Edit a file on a remote host via SSH."
  (interactive)
  (let* ((hostname (read-string "Hostname/IP: "))
         (username (read-string "Username: "))
         (port (read-string "Port (default 22): " nil nil "22"))
         (filepath (read-string "Remote file path: " "/home/"))
         (remote-file (format "/ssh:%s@%s#%s:%s" username hostname port filepath)))
    
    ;; Validate inputs
    (when (or (string-empty-p hostname) (string-empty-p username))
      (error "Hostname and username are required"))
    
    (message "Opening file %s on %s@%s:%s..." filepath username hostname port)
    
    ;; Open the remote file
    (find-file remote-file)))

(defun ssh-quick-connect ()
  "Quick SSH connection using a predefined format."
  (interactive)
  (let* ((connection-string (read-string "Connection (user@host:port): "))
         (parts (split-string connection-string "[@ :]"))
         (username (nth 0 parts))
         (hostname (nth 1 parts))
         (port (if (nth 2 parts) (nth 2 parts) "22"))
         (remote-path (format "/ssh:%s@%s#%s:/" username hostname port)))
    
    ;; Basic validation
    (when (< (length parts) 2)
      (error "Invalid format. Use: user@host or user@host:port"))
    
    (message "Connecting to %s..." connection-string)
    
    ;; Open dired on remote host
    (dired remote-path)))

;; Optional: Install hydra package for the hydra menu
;; To use hydra, install it first: M-x package-install RET hydra RET
;; Then uncomment the following code:

;; (when (fboundp 'defhydra)
;;   (defhydra hydra-ssh (:color blue :hint nil)
;;     "
;; ^SSH Connections^
;; ^^^^^^^^-----------
;; _c_: Connect (dired)
;; _s_: Connect (shell)  
;; _t_: Connect (terminal)
;; _f_: Edit file
;; _q_: Quick connect
;; _x_: Cancel
;; "
;;     ("c" ssh-connect)
;;     ("s" ssh-connect-with-shell)
;;     ("t" ssh-connect-terminal)
;;     ("f" ssh-edit-file)
;;     ("q" ssh-quick-connect)
;;     ("x" nil)))

;; Alternative simple menu using completing-read
(defun ssh-menu ()
  "SSH connection menu."
  (interactive)
  (let ((choice (completing-read "SSH Action: " 
                                '("Connect (dired)"
                                  "Connect (shell)"
                                  "Connect (terminal)"
                                  "Edit file"
                                  "Quick connect"))))
    (cond
     ((string= choice "Connect (dired)") (ssh-connect))
     ((string= choice "Connect (shell)") (ssh-connect-with-shell))
     ((string= choice "Connect (terminal)") (ssh-connect-terminal))
     ((string= choice "Edit file") (ssh-edit-file))
     ((string= choice "Quick connect") (ssh-quick-connect)))))

;; Key bindings (optional)
;; Uncomment and modify as needed:
;; (global-set-key (kbd "C-c s c") 'ssh-connect)
;; (global-set-key (kbd "C-c s s") 'ssh-connect-with-shell)
;; (global-set-key (kbd "C-c s m") 'ssh-menu)
;; If you have hydra installed:
;; (global-set-key (kbd "C-c s h") 'hydra-ssh/body)

(provide 'ssh-connect)

;;; ssh-connect.el ends here
