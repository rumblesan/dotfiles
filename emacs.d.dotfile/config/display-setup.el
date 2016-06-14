(require 'packages)

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(require 'moe-theme)
(moe-dark)

(require 'rumblesan-powerline)

;; Don't display the menu bar, tool bar or scroll bar
(menu-bar-mode -1)
(if (display-graphic-p)
  (progn
    (tool-bar-mode -1)
    (scroll-bar-mode -1)
    ))

(set-frame-font "Menlo for Powerline-14")

(setq initial-scratch-message nil)
(setq inhibit-startup-screen t)

(provide 'display-setup)
