(ensure-package-installed 'color-theme-solarized)

(setq frame-background-mode 'dark)

(load-theme 'solarized t)

(enable-theme 'solarized)

(ensure-package-installed 'spaceline)
(require 'spaceline)
(require 'spaceline-config)
(spaceline-spacemacs-theme)
(spaceline-helm-mode)

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
