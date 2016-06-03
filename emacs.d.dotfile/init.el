;; Don't display the menu bar, tool bar or scroll bar
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq inhibit-startup-screen t)

(setq my-lisp-dir
    (expand-file-name "lisp" user-emacs-directory))

(add-to-list 'load-path my-lisp-dir)

(set-frame-font "Menlo for Powerline-14")

(setq make-backup-files nil)

(setq initial-scratch-message nil)

(global-linum-mode)

;;; Prevent Extraneous Tabs
(setq-default indent-tabs-mode nil)

(when (eq system-type 'darwin)
  (require 'osx-tweaks)
  )

(require 'packages)

(require 'display-setup)

(require 'evil-setup)
