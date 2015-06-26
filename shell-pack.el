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
    (dolist (var '(;; ssh/gpg agent
                   "SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "GPG_TTY"
                   ;; lang
                   "LANG" "LC_CTYPE"
                   ;; library
                   "LD_LIBRARY_PATH"
                   ;; proxy
                   "http_proxy" "https_proxy" "ftp_proxy" "rsync_proxy" "no_proxy"))
      (add-to-list 'exec-path-from-shell-variables var))))

(defun shell-pack/load-environment-within-emacs ()
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

(defun shell-pack/mode-and-simple-bindings-fn ()
  "Simple binding definition and add smartscan mode."
  (local-set-key (kbd "C-c C-j") 'term-line-mode))


(defun shell-pack/close-buffer-hook-fn ()
  "Hook function to kill the buffer given a specific binding."
  (local-set-key (kbd "C-d") 'comint-delchar-or-eof-or-kill-buffer))

(defun ash-term-hooks ()
  "A hook to permit to yank in `'ansi-term`'."
  (define-key term-raw-escape-map (kbd "C-y") (lambda ()
                                                (interactive)
                                                (term-send-raw-string (current-kill 0)))))



(dolist (hook '(sh-mode-hook term-mode-hook shell-mode-hook eshell-mode-hook))
  (add-hook hook 'shell-pack/mode-and-simple-bindings-fn))

(dolist (hook '(term-mode-hook shell-mode-hook eshell-mode-hook))
  (add-hook hook 'shell-pack/close-buffer-hook-fn))

(add-hook 'term-mode-hook 'ash-term-hooks)

;; Create a global shell pack mode

(defvar shell-pack-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c s l") 'shell-pack/load-environment-within-emacs)
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

(shell-pack/load-environment-within-emacs)

(provide 'shell-pack)
;;; shell-pack.el ends here
