;;; shell-pack.el --- shell configuration

;;; Commentary:

;;; Code:

(require 'install-packages-pack)
(install-packages-pack/install-packs '(multi-term
                                       exec-path-from-shell
                                       smartscan))

(require 'smartscan)
(require 'term)
(require 'shell)

;; setup the path
(require 'exec-path-from-shell)

;; Add some env variables so that emacs is aware too
(eval-after-load 'exec-path-from-shell
  (lambda ()
    (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE" "LD_LIBRARY_PATH" "http_proxy" "https_proxy" "ftp_proxy" "rsync_proxy" "no_proxy"))
      (add-to-list 'exec-path-from-shell-variables var))))

(exec-path-from-shell-initialize)

(defun comint-delchar-or-eof-or-kill-buffer (arg)
  "Delete ARG char or kill buffer if we hit the end of the file."
  (interactive "p")
  (if (null (get-buffer-process (current-buffer)))
      (kill-buffer)
    (comint-delchar-or-maybe-eof arg)))

(defun shell-pack/mode-and-simple-bindings-fn ()
  "Simple binding definition and add smartscan mode."
  (local-set-key (kbd "C-c C-j") 'term-line-mode))

(dolist (hook '(sh-mode-hook term-mode-hook shell-mode-hook eshell-mode-hook))
  (add-hook hook 'shell-pack/mode-and-simple-bindings-fn))

(defun shell-pack/close-buffer-hook-fn ()
  "Hook function to kill the buffer given a specific binding."
  (local-set-key (kbd "C-d") 'comint-delchar-or-eof-or-kill-buffer))

(dolist (hook '(term-mode-hook shell-mode-hook eshell-mode-hook))
  (add-hook hook 'shell-pack/close-buffer-hook-fn))

(provide 'shell-pack)
;;; shell-pack.el ends here
