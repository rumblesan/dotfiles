(setq my-lisp-dir
    (expand-file-name "lisp" user-emacs-directory))

(add-to-list 'load-path my-lisp-dir)

(setq make-backup-files nil)

;;; Prevent Extraneous Tabs
(setq-default indent-tabs-mode nil)

(when (eq system-type 'darwin)
  (require 'osx-tweaks)
  )

(require 'packages)

(require 'display-setup)

(require 'evil-setup)
(require 'tmux-navigate)
