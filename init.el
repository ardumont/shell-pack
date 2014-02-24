(install-packs '(;; multi-term
                 multi-term))

(require 'term)
(require 'shell)

(add-hook 'term-mode-hook (function () (define-key term-mode-map (kbd "C-c C-j") 'term-line-mode)))

;; With this snippet, another press of C-d will kill the buffer.
(defun comint-delchar-or-eof-or-kill-buffer (arg)
  (interactive "p")
  (if (null (get-buffer-process (current-buffer)))
      (kill-buffer)
    (comint-delchar-or-maybe-eof arg)))

(add-hook 'shell-mode-hook
          (lambda ()
            (define-key shell-mode-map
              (kbd "C-d") 'comint-delchar-or-eof-or-kill-buffer)))
