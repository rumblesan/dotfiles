;;; -*- Mode: emacs-lisp; -*-

;; Don't display the menu bar, tool bar or scroll bar
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq inhibit-startup-screen t)

(setq my-lisp-dir
    (expand-file-name "lisp" user-emacs-directory))

(add-to-list 'load-path my-lisp-dir)

(setq make-backup-files nil)

(setq initial-scratch-message nil)

;;; Prevent Extraneous Tabs
(setq-default indent-tabs-mode nil)

(when (eq system-type 'darwin)
  (require 'osx-tweaks)
  )

(require 'packages)

(require 'display-setup)

(require 'helm-setup)

(require 'evil-setup)
