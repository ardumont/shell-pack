;;; shell-pack.el --- shell configuration

;;; Commentary:

;;; Code:

(install-packs '(multi-term
                 exec-path-from-shell
                 smartscan))

(require 'smartscan)
(require 'term)
(require 'shell)

;; setup the path
(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)

;; With this snippet, another press of C-d will kill the buffer.
(defun comint-delchar-or-eof-or-kill-buffer (arg)
  (interactive "p")
  (if (null (get-buffer-process (current-buffer)))
      (kill-buffer)
    (comint-delchar-or-maybe-eof arg)))

(defun shell-pack/hook-fn ()
  "Deactivate smartscan to be able to parse history"
  (local-set-key (kbd "C-c C-j") 'term-line-mode)
  (local-set-key (kbd "C-d") 'comint-delchar-or-eof-or-kill-buffer)
  (smartscan-mode))

(dolist (hook '(sh-mode-hook term-mode-hook shell-mode-hook eshell-mode-hook))
  (add-hook hook 'shell-pack/hook-fn))

;;; shell-pack.el ends here
