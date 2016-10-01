;;; init.el --- Set it up!


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

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
  )


(require 'packages)

(require 'util)

(require 'display-setup)

(require 'navigation)

(require 'development)

(custom-set-variables
 '(package-selected-packages
   (quote
    (evil-magit magit coffee-mode js2-mode markdown-mode cmake-mode haskell-mode ensime sbt-mode scala-mode flycheck evil-surround general hydra shackle helm rainbow-delimiters powerline-evil moe-theme powerline use-package))))
(custom-set-faces)
