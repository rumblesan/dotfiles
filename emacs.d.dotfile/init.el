;;; init.el --- Set it up!

(add-to-list 'load-path
             (expand-file-name "config" user-emacs-directory))

;; Always ALWAYS use UTF-8
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(load-library "iso-transl")

;; Don't warn about advice function redefenitions
(setq ad-redefinition-action 'accept)

;; Always ask for y/n keypress instead of typing out 'yes' or 'no'
(defalias 'yes-or-no-p 'y-or-n-p)
;; Backup file functionality
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(setq create-lockfiles nil)
;; Turn bell off entirely
(setq ring-bell-function 'ignore)
;; Prevent Extraneous Tabs
(setq-default indent-tabs-mode nil)
;; Mouse settings
(unless window-system
  (require 'mouse)
  (xterm-mouse-mode t)
  (global-set-key [mouse-4] (lambda ()
                              (interactive)
                              (scroll-down 1)))
  (global-set-key [mouse-5] (lambda ()
                              (interactive)
                              (scroll-up 1)))
  (defun track-mouse (e))
  (if (boundp 'mouse-sel-mode)
    (setq mouse-sel-mode t))
  )


(require 'packages)

(require 'display-setup)

(require 'navigation)

(require 'development)

(defvar rumblesan/scdir (expand-file-name "site-lisp/sc/" user-emacs-directory))

;; Add SuperCollider config
(if (file-exists-p rumblesan/scdir)
    (progn
      (message "loading sc")
      (add-to-list 'load-path rumblesan/scdir)
      (require 'sclang)
      ))
