(require 'packages)

(ensure-package-installed 'moe-theme
                          'rainbow-delimiters)

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(setq frame-background-mode 'dark)

(require 'moe-theme)
(moe-dark)

(ensure-package-installed 'spaceline)
(require 'spaceline)
(require 'spaceline-segments)
(require 'spaceline-config)

(setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
(spaceline-install 'rumblesan
                   `((evil-state :face highlight-face)
                     auto-compile
                     '(buffer-modified buffer-size buffer-id remote-host)
                     major-mode
                     (process :when active)
                     ((flycheck-error flycheck-warning flycheck-info)
                      :when active)
                     (minor-modes :when active)
                     (mu4e-alert-segment :when active)
                     (erc-track :when active)
                     (version-control :when active)
                     (org-pomodoro :when active)
                     (org-clock :when active))

                   `(which-function
                     (python-pyvenv :fallback python-pyenv)
                     (battery :when active)
                     selection-info
                     input-method
                     ((buffer-encoding-abbrev
                       point-position
                       line-column)
                      :separator " | ")
                     (global :when active)
                     buffer-position
                     hud))

(setq-default mode-line-format '("%e" (:eval (spaceline-ml-rumblesan))))


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
