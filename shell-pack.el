;;; shell-pack.el --- shell configuration

;;; Commentary:

;;; Code:

(use-package smartscan)
(use-package exec-path-from-shell)
(use-package term)
(use-package shell)
(use-package popwin)

(defun shell-pack-log (&rest args)
  "Log the message ARGS in the mini-buffer."
  (apply #'message (format "Shell Pack - %s" (car args)) (cdr args)))

(defvar shell-pack--variables-to-forward-to-emacs
  '(;; ssh/gpg agent
    "SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "GPG_TTY"
    ;; lang
    "LANG" "LC_CTYPE"
    ;; library
    "LD_LIBRARY_PATH"
    ;; proxy
    "http_proxy" "https_proxy" "ftp_proxy" "rsync_proxy" "no_proxy")
  "The variables that are worth maintaining alive in Emacs session.")

;; setup the path
(use-package exec-path-from-shell
  :config
  (dolist (var shell-pack--variables-to-forward-to-emacs)
    (add-to-list 'exec-path-from-shell-variables var)))

(defun shell-pack-run-shell (&rest cmd)
  "A simpler command CMD to run-shell-command with multiple params."
  (shell-command-to-string (apply #'concatenate 'string cmd)))

(defun shell-pack-show-env ()
  "Display the current environment variables."
  (interactive)
  (-when-let (msg (with-temp-buffer
                    (->> shell-pack--variables-to-forward-to-emacs
                         (--map (format "%s: %s" it (getenv it)))
                         (s-join "\n")
                         insert)
                    (goto-char (point-min))
                    (align-regexp (point-min) (point-max) "\\(:\\)" 1 0)
                    (buffer-string)))
    (message msg)))

(defun shell-pack-load-environment-within-emacs ()
  "Reload the environment variables from the shell env."
  (interactive)
  (exec-path-from-shell-initialize))

;; Extend shell-mode with some bindings

(defun comint-delchar-or-eof-or-kill-buffer (arg)
  "Delete ARG char or kill buffer if we hit the end of the file."
  (interactive "p")
  (if (null (get-buffer-process (current-buffer)))
      (kill-buffer)
    (comint-delchar-or-maybe-eof arg)))

(defun shell-pack-mode-and-simple-bindings-fn ()
  "Simple binding definition and add smartscan mode."
  (local-set-key (kbd "C-c C-j") 'term-line-mode))


(defun shell-pack-close-buffer-hook-fn ()
  "Hook function to kill the buffer given a specific binding."
  (local-set-key (kbd "C-d") 'comint-delchar-or-eof-or-kill-buffer))

(defun shell-pack-ash-term-hooks ()
  "A hook to permit to yank in `'ansi-term`'."
  (define-key term-raw-escape-map (kbd "C-y") (lambda ()
                                                (interactive)
                                                (term-send-raw-string (current-kill 0)))))



(dolist (hook '(sh-mode-hook term-mode-hook shell-mode-hook eshell-mode-hook))
  (add-hook hook 'shell-pack-mode-and-simple-bindings-fn))

(dolist (hook '(term-mode-hook shell-mode-hook eshell-mode-hook))
  (add-hook hook 'shell-pack-close-buffer-hook-fn))

(add-hook 'term-mode-hook 'shell-pack-ash-term-hooks)

;; Create a global shell pack mode

(defvar shell-pack-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c s l") 'shell-pack-load-environment-within-emacs)
    (define-key map (kbd "C-c s d") 'shell-pack-show-env)
    map)
  "Keymap for Shell-pack mode.")

(define-minor-mode shell-pack-mode
  "Minor mode to consolidate Emacs' shell extensions.

\\{shell-pack-mode-map}"
  :lighter " SP"
  :keymap shell-pack-mode-map)

(define-globalized-minor-mode global-shell-pack-mode shell-pack-mode shell-pack-on)

(defun shell-pack-on ()
  "Turn on `shell-pack-mode'."
  (shell-pack-mode +1))

(global-shell-pack-mode)

(global-prettify-symbols-mode 1)

(shell-pack-load-environment-within-emacs)

(provide 'shell-pack)
;;; shell-pack.el ends here
