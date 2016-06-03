(ensure-package-installed 'evil 'evil-leader)
(require 'evil)
(require 'evil-leader)

(evil-leader/set-leader ",")

(evil-leader/set-key
  "f" 'helm-find-files
  "b" 'switch-to-buffer
  "k" 'kill-buffer)

(setq evil-shift-width 8)

(global-evil-leader-mode)
(evil-mode t)

(provide 'evil-setup)
