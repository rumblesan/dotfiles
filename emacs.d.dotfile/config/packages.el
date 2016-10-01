;;; packages.el --- Useful stuff for setting up packages

(require 'package)
(setq package-enable-at-startup nil)
(dolist (source '(("org" . "http://orgmode.org/elpa")
                  ("gnu" . "https://elpa.gnu.org/packages/")
                  ("melpa" . "http://melpa.org/packages/")
                  ("marmalade" . "http://marmalade-repo.org/packages/")
                  ("elpa" . "http://tromey.com/elpa/")))
  (add-to-list 'package-archives source t))

(setq use-package-always-ensure t)
(package-initialize)

;; Bootstrap 'use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(provide 'packages)
