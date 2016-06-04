(ensure-package-installed 'color-theme-solarized)

(setq frame-background-mode 'dark)

(load-theme 'solarized t)

(enable-theme 'solarized)

;; Don't display the menu bar, tool bar or scroll bar
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(set-frame-font "Menlo for Powerline-14")

(setq initial-scratch-message nil)
(setq inhibit-startup-screen t)


(global-linum-mode)


(provide 'display-setup)
