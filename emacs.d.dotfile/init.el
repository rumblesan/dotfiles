;;; -*- Mode: emacs-lisp; -*-

;; Don't display the menu bar, tool bar or scroll bar
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq inhibit-startup-screen t)

(setq my-lisp-dir
    (expand-file-name "lisp" user-emacs-directory))

(add-to-list 'load-path my-lisp-dir)

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; key bindings
(when (eq system-type 'darwin) ;; mac specific settings
            (setq mac-option-modifier 'alt)
            (setq mac-command-modifier 'meta)
            (global-set-key [kp-delete] 'delete-char) ;; sets fn-delete to be right-delete
        )


(setq initial-scratch-message nil)

(require 'packages)

(load-theme 'solarized-dark t)

