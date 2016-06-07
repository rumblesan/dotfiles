
(add-to-list 'load-path
             (expand-file-name "config" user-emacs-directory))

;;; Backup file functionality
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
;;; Turn bell off entirely
(setq ring-bell-function 'ignore)
;;; Prevent Extraneous Tabs
(setq-default indent-tabs-mode nil)
;;; Mouse settings
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
