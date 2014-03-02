;;; shell-pack.el --- shell configuration

;;; Commentary:

;;; Code:

(install-packs '(multi-term
                 exec-path-from-shell))

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

(defun exists-session-or-spawn-it (session-name session-command)
  "Given a session-name, check the existence of such a session. If it doesn't exist, spawn the session via the command session-command"
  (let ((proc (get-buffer-process session-name)))
    (unless (and proc (eq (process-status proc) 'run))
      (funcall session-command))))

(defun switch-to-buffer-or-nothing (process-name buffer-name)
  "Given a process name, switch to the corresponding buffer-name (if the process is running) or does nothing."
  (unless (string= (buffer-name) buffer-name)
    (let ((proc (get-buffer-process process-name)))
      (if (and proc (eq (process-status proc) 'run))
          (switch-to-buffer buffer-name)))))

;; examples
;; (switch-to-buffer-or-nothing "*swank*" "*slime-repl nil*")    ;; clojure-jack-in
;; (switch-to-buffer-or-nothing "*terminal<1>*" "*terminal<1>*") ;; multi-term

(defun multi-term-once ()
  "Check the existence of a terminal with multi-term.
If it doesn't exist, launch it. Then go to this buffer in another buffer."
  (interactive)
  (unless (exists-session-or-spawn-it "*terminal<1>*" 'multi-term)
    (switch-to-buffer-or-nothing "*terminal<1>*" "*terminal<1>*")))

(defun shell-pack/--smartscan-off ()
  "Deactivate smartscan to be able to parse history"
  (and (fboundp 'smartscan-mode) smartscan-mode (smartscan-mode -1)))

(add-hook 'term-mode-hook
          (lambda ()
            (define-key term-mode-map (kbd "C-c C-j") 'term-line-mode)
            (define-key term-mode-map (kbd "C-c C-z") 'multi-term-once)
            (shell-pack/--smartscan-off)))

(add-hook 'shell-mode-hook
          (lambda ()
            (define-key shell-mode-map
              (kbd "C-d") 'comint-delchar-or-eof-or-kill-buffer)
            (shell-pack/--smartscan-off)))


;;; shell-pack.el ends here
