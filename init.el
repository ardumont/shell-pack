;;; shell-pack.el --- shell configuration

;;; Commentary:

;;; Code:

(install-packs '(multi-term
                 exec-path-from-shell
                 smartscan-mode))

(require 'smartscan-mode)
(add-to-list 'auto-mode-alist '("\.sh$" . smartscan-mode))
(add-to-list 'auto-mode-alist '("\.zsh$" . smartscan-mode))

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

(defun shell-pack/--smartscan-off ()
  "Deactivate smartscan to be able to parse history"
  (local-set-key (kbd "C-c C-j") 'term-line-mode)
  (local-set-key (kbd "C-d") 'comint-delchar-or-eof-or-kill-buffer)
  (and (fboundp 'smartscan-mode) smartscan-mode (smartscan-mode -1)))

(add-hook 'term-mode-hook 'shell-pack/--smartscan-off)
(add-hook 'shell-mode-hook 'shell-pack/--smartscan-off)
(add-hook 'eshell-mode-hook 'shell-pack/--smartscan-off)

;;; shell-pack.el ends here
