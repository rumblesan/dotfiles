(require 'packages)

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(require 'moe-theme)
(moe-dark)

(require 'rumblesan-powerline)

;; Don't display the menu bar, tool bar or scroll bar
(dolist (mode '(menu-bar-mode
                tool-bar-mode
                scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

(set-frame-font "Menlo for Powerline-14")

(setq initial-scratch-message nil)
(setq inhibit-startup-screen t)

(provide 'display-setup)
