;;; packages.el --- Useful stuff for setting up packages

(require 'package)

;; Detect online status, from ESK
(require 'cl)
(defun esk-online? ()
  (if (and (functionp 'network-interface-list)
           (network-interface-list))
      (some (lambda (iface) (unless (equal "lo" (car iface))
                         (member 'up (first (last (network-interface-info
                                                   (car iface)))))))
            (network-interface-list))
    t))

(dolist (source '(("org" . "http://orgmode.org/elpa")
                  ("gnu" . "https://elpa.gnu.org/packages/")
                  ("melpa" . "http://melpa.org/packages/")
                  ("marmalade" . "http://marmalade-repo.org/packages/")
                  ("elpa" . "http://tromey.com/elpa/")))
  (add-to-list 'package-archives source t))

(setq use-package-always-ensure t)
(package-initialize)
(when (esk-online?)
  (unless package-archive-contents
    (package-refresh-contents)
    (package-install 'use-package)
    ))

(require 'use-package)
(provide 'packages)
