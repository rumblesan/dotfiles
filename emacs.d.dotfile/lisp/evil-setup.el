(ensure-package-installed 'evil 'evil-leader)
(require 'evil)
(require 'evil-leader)

(evil-leader/set-leader ",")

(evil-leader/set-key
  "f" 'helm-find-files
  "b" 'switch-to-buffer
  "k" 'kill-buffer)

(define-key evil-normal-state-map "\C-h" 'evil-window-left)
(define-key evil-normal-state-map "\C-j" 'evil-window-down)
(define-key evil-normal-state-map "\C-k" 'evil-window-up)
(define-key evil-normal-state-map "\C-l" 'evil-window-right)

(define-key evil-motion-state-map ";" 'evil-ex)

(setq evil-shift-width 4)

(setq evil-vsplit-window-right 'left)
(setq evil-split-window-below 'above)

(global-evil-leader-mode)
(evil-mode t)

(provide 'evil-setup)
